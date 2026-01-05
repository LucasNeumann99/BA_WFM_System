#!/usr/bin/env Rscript
# ============================================================
# 21_prophet_vs_glm_compare.R
# ------------------------------------------------------------
# Compare Prophet POC vs GLM metrics and trend signals.
# Outputs to: output/analysis_extra/prophet_poc/compare
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)
library(prophet)

source(here("model_functions", "paths.R"))

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
paths <- get_pipeline_paths()
base_out <- file.path(paths$output, "analysis_extra", "prophet_poc")
out_dir <- file.path(base_out, "compare")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

glm_metrics_path <- file.path(paths$output, "analysis_extra", "final_glm_diagnostics", "metrics_final_glm.csv")
prophet_metrics_path <- file.path(base_out, "metrics_prophet_poc.csv")
glm_fc_path <- file.path(paths$results, "final", "glm", "fc_final_glm_negbin.rds")

glm_trend_path <- file.path(paths$output, "analysis_extra", "model_story", "team_annual_trend.csv")

prophet_models_path <- here("models", "prophet_poc_by_team.rds")

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

df_base <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))

holiday_regs <- c(
  "Juleferie", "Vinterferie", "Påskeferie", "Sommerferie", "Efterårsferie"
)

team_colors <- c(
  "Team DK 1, Travelcare" = "#D93945",
  "Team SE 1, Travelcare" = "#3E3E3E",
  "Team SE 2, Travelcare" = "#2E9A5D",
  "Team NO 1, Travelcare" = "#2A6F97",
  "Team FI 1, Travelcare" = "#8E5C9E",
  "Team SE total" = "#E3B23C"
)

# ------------------------------------------------------------
# Metrics comparison
# ------------------------------------------------------------
compute_metrics <- function(df) {
  df %>%
    summarise(
      n = sum(!is.na(y_hat) & !is.na(y)),
      RMSE = sqrt(mean((y - y_hat)^2)),
      MAE  = mean(abs(y - y_hat)),
      MAPE = mean(abs(y - y_hat) / pmax(y, 1)),
      SMAPE = mean(abs(y - y_hat) / ((abs(y) + abs(y_hat)) / 2)),
      Bias_mean = mean(y_hat - y),
      Bias_sd   = sd(y_hat - y)
    )
}

if (file.exists(glm_metrics_path)) {
  glm_metrics <- readr::read_csv(glm_metrics_path, show_col_types = FALSE) %>%
    select(team, RMSE, MAE, MAPE, SMAPE, Bias_mean, Bias_sd, model) %>%
    mutate(model = "GLM")
} else if (file.exists(glm_fc_path)) {
  glm_fc <- readRDS(glm_fc_path)
  glm_metrics <- glm_fc %>%
    group_by(team) %>%
    group_modify(~ compute_metrics(.x)) %>%
    ungroup() %>%
    mutate(model = "GLM")
} else {
  stop("Missing GLM metrics and forecast RDS. Run pipeline/14_train_final_glm.R first.")
}

prophet_metrics <- readr::read_csv(prophet_metrics_path, show_col_types = FALSE) %>%
  select(team, RMSE, MAE, MAPE, SMAPE, Bias_mean, Bias_sd, model) %>%
  mutate(model = "Prophet")

metrics_compare <- glm_metrics %>%
  rename_with(~ paste0(.x, "_glm"), -c(team, model)) %>%
  select(-model) %>%
  left_join(
    prophet_metrics %>% rename_with(~ paste0(.x, "_prophet"), -c(team, model)) %>% select(-model),
    by = "team"
  ) %>%
  mutate(
    delta_RMSE = RMSE_prophet - RMSE_glm,
    delta_MAE = MAE_prophet - MAE_glm,
    delta_MAPE = MAPE_prophet - MAPE_glm,
    delta_SMAPE = SMAPE_prophet - SMAPE_glm,
    delta_Bias = Bias_mean_prophet - Bias_mean_glm
  ) %>%
  filter(team != "Team SE total")

readr::write_csv(metrics_compare, file.path(out_dir, "metrics_prophet_vs_glm.csv"))

prophet_wins <- sum(metrics_compare$delta_RMSE < 0, na.rm = TRUE)
glm_wins <- sum(metrics_compare$delta_RMSE > 0, na.rm = TRUE)

rmse_title <- if (prophet_wins > glm_wins) {
  paste0("Prophet vinder RMSE for ", prophet_wins, " af ", prophet_wins + glm_wins, " teams")
} else if (glm_wins > prophet_wins) {
  paste0("GLM vinder RMSE for ", glm_wins, " af ", prophet_wins + glm_wins, " teams")
} else {
  "RMSE er delt mellem Prophet og GLM"
}

plot_metrics <- metrics_compare %>%
  mutate(team = fct_reorder(team, delta_RMSE)) %>%
  ggplot(aes(x = team, y = delta_RMSE, fill = team)) +
  geom_col() +
  scale_fill_manual(values = team_colors, na.translate = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = rmse_title,
    x = "Team",
    y = "Delta RMSE"
  )

ggsave(
  file.path(out_dir, "delta_rmse_prophet_vs_glm.png"),
  plot = plot_metrics,
  width = 9,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Trend comparison
# ------------------------------------------------------------
glm_trend <- readr::read_csv(glm_trend_path, show_col_types = FALSE)
if ("annual_trend_pct" %in% names(glm_trend)) {
  glm_trend <- glm_trend %>%
    select(team, annual_trend_pct) %>%
    mutate(source = "GLM")
} else if ("annual_growth_pct" %in% names(glm_trend)) {
  glm_trend <- glm_trend %>%
    select(team, annual_growth_pct) %>%
    rename(annual_trend_pct = annual_growth_pct) %>%
    mutate(source = "GLM")
} else {
  stop("GLM trend file missing annual trend column: ", glm_trend_path)
}

prophet_models <- readRDS(prophet_models_path)

prophet_trend <- imap_dfr(prophet_models, function(mod, tm) {
  if (!inherits(mod, "prophet")) return(NULL)
  
  if (tm == "Team SE total") {
    df_tm <- df_base %>%
      filter(team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare")) %>%
      group_by(ds) %>%
      summarise(
        y = sum(y, na.rm = TRUE),
        across(all_of(holiday_regs), ~ first(.x)),
        .groups = "drop"
      ) %>%
      mutate(team = tm)
  } else {
    df_tm <- df_base %>% filter(team == tm)
  }
  
  if (nrow(df_tm) == 0) return(NULL)
  
  df_test <- df_tm %>% filter(ds >= test_start_dt, ds <= test_end_dt)
  if (nrow(df_test) == 0) return(NULL)
  
  df_future <- df_test %>%
    transmute(
      ds = ds,
      across(all_of(holiday_regs), ~ as.numeric(.x))
    )
  
  preds <- predict(mod, df_future)
  
  trend_df <- tibble(
    ds = df_future$ds,
    trend = preds$trend
  ) %>%
    mutate(t = as.numeric(ds))
  
  if (nrow(trend_df) < 2) return(NULL)
  
  trend_fit <- lm(trend ~ t, data = trend_df)
  slope <- coef(trend_fit)[["t"]]
  
  year_seconds <- 365.25 * 24 * 3600
  annual_change <- slope * year_seconds
  annual_trend_pct <- (exp(annual_change) - 1) * 100
  
  if (!is.finite(annual_trend_pct)) annual_trend_pct <- NA_real_
  
  tibble(team = tm, annual_trend_pct = annual_trend_pct, source = "Prophet")
})

if (any(prophet_trend$team == "Team SE total")) {
  se_total_trend <- prophet_trend %>% filter(team == "Team SE total")
  prophet_trend <- bind_rows(
    prophet_trend,
    se_total_trend %>% mutate(team = "Team SE 1, Travelcare"),
    se_total_trend %>% mutate(team = "Team SE 2, Travelcare")
  ) %>%
    distinct(team, source, .keep_all = TRUE)
}

trend_compare <- bind_rows(glm_trend, prophet_trend) %>%
  filter(team != "Team SE total")
readr::write_csv(trend_compare, file.path(out_dir, "annual_trend_prophet_vs_glm.csv"))

trend_plot <- trend_compare %>%
  filter(!is.na(annual_trend_pct)) %>%
  mutate(team = fct_reorder(team, annual_trend_pct)) %>%
  ggplot(aes(x = team, y = annual_trend_pct, fill = source)) +
  geom_col(position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("GLM" = "#3E3E3E", "Prophet" = "#D93945")) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Årlig trend: Prophet vs GLM",
    x = "Team",
    y = "Annual trend (%)",
    fill = ""
  )

ggsave(
  file.path(out_dir, "annual_trend_prophet_vs_glm.png"),
  plot = trend_plot,
  width = 9,
  height = 5,
  dpi = 300
)

message("Prophet vs GLM comparison saved to: ", out_dir)
