#!/usr/bin/env Rscript
# ============================================================
# 31_prophet_trend_vs_glm.R
# ------------------------------------------------------------
# Compare Prophet trend component vs GLM linear year trend.
# Output: BA_WFM_Output/analysis_extra/prophet_poc/compare
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)
library(prophet)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "prophet_poc", "compare")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

prophet_models_path <- here("models", "prophet_poc_by_team.rds")
if (!file.exists(prophet_models_path)) {
  stop("Missing prophet_poc_by_team.rds. Run analysis_extra/20_prophet_poc_forecast.R first.")
}

models_glm <- readRDS(here("models", "final_glm_negbin_by_team.rds"))
prophet_models <- readRDS(prophet_models_path)

df_base <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))

cfg <- fromJSON(here("config", "forecast_v2.json"))
tz_info <- if (!is.null(cfg$timezone) && nzchar(cfg$timezone)) cfg$timezone else "UTC"
eval_cfg <- if (!is.null(cfg$evaluation)) cfg$evaluation else list()

test_start_val <- if (!is.null(eval_cfg$test_start) && nzchar(eval_cfg$test_start)) {
  eval_cfg$test_start
} else {
  "2025-01-01 00:00:00"
}
test_end_val <- if (!is.null(eval_cfg$test_end) && nzchar(eval_cfg$test_end)) {
  eval_cfg$test_end
} else {
  ""
}

test_start_dt <- ymd_hms(test_start_val, tz = tz_info)
test_end_dt <- ymd_hms(test_end_val, tz = tz_info)
if (is.na(test_end_dt)) {
  test_end_dt <- max(df_base$ds)
}

holiday_regs <- c(
  "Juleferie", "Vinterferie", "Påskeferie", "Sommerferie", "Efterårsferie"
)

build_prophet_trend <- function(team_name, df_team) {
  mod <- prophet_models[[team_name]]
  if (is.null(mod) && team_name %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare")) {
    mod <- prophet_models[["Team SE total"]]
  }
  if (is.null(mod)) return(NULL)
  
  df_team <- df_team %>%
    filter(ds >= test_start_dt, ds <= test_end_dt) %>%
    transmute(
      ds = ds,
      across(all_of(holiday_regs), ~ as.numeric(.x))
    )
  
  if (nrow(df_team) == 0) return(NULL)
  
  preds <- predict(mod, df_team)
  tibble(ds = df_team$ds, trend = exp(preds$trend) - 1)
}

build_glm_trend <- function(team_name, df_team) {
  mod <- models_glm[[team_name]]
  if (is.null(mod)) return(NULL)
  
  coef_year <- coef(mod)[["year_c"]]
  df_team <- df_team %>%
    filter(ds >= test_start_dt, ds <= test_end_dt) %>%
    mutate(year_c = as.numeric(year) - 2024)
  
  if (nrow(df_team) == 0) return(NULL)
  
  trend_factor <- exp(coef_year * df_team$year_c)
  tibble(ds = df_team$ds, trend = trend_factor)
}

teams <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare",
  "Team SE 2, Travelcare"
)

trend_series <- list()
trend_summary <- list()

for (tm in teams) {
  df_tm <- df_base %>% filter(team == tm)
  
  prop_trend <- build_prophet_trend(tm, df_tm)
  glm_trend <- build_glm_trend(tm, df_tm)
  
  if (is.null(prop_trend) || is.null(glm_trend)) next
  
  trend_series[[tm]] <- bind_rows(
    prop_trend %>% mutate(team = tm, source = "Prophet"),
    glm_trend %>% mutate(team = tm, source = "GLM")
  )
  
  # Annualized slope (linear on log-trend)
  annual_slope <- function(df) {
    df <- df %>% mutate(t = as.numeric(ds))
    fit <- lm(log(trend) ~ t, data = df)
    slope <- coef(fit)[["t"]]
    year_seconds <- 365.25 * 24 * 3600
    (exp(slope * year_seconds) - 1) * 100
  }
  
  trend_summary[[tm]] <- tibble(
    team = tm,
    glm_trend_pct = annual_slope(glm_trend),
    prophet_trend_pct = annual_slope(prop_trend)
  )
}

trend_series_df <- bind_rows(trend_series)
trend_summary_df <- bind_rows(trend_summary)

readr::write_csv(trend_summary_df, file.path(out_dir, "prophet_vs_glm_trend_summary.csv"))

p <- trend_series_df %>%
  ggplot(aes(ds, trend, color = source)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ team) +
  scale_color_manual(values = c("GLM" = "#3E3E3E", "Prophet" = "#D93945")) +
  labs(
    title = "Trend comparison: Prophet vs GLM",
    subtitle = "Raw trend scale (no normalization)",
    x = "Date",
    y = "Trend index",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(out_dir, "prophet_vs_glm_trend_comparison.png"),
  p,
  width = 10,
  height = 5.5,
  dpi = 300
)

message("Saved trend comparison to: ", out_dir)
