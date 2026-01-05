#!/usr/bin/env Rscript
# ============================================================
# 23_team_no_bias_three_variants.R
# ------------------------------------------------------------
# Compare Team NO bias before Storebrand removal, after removal,
# and after level adjustment (pre-2025-06 scaling).
# Outputs to: BA_WFM_Output/analysis_extra/Strukturelle ændringer/Team_no_customer_exit
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)
library(MASS)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "Strukturelle ændringer", "Team_no_customer_exit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

team_no <- "Team NO 1, Travelcare"
storebrand_code <- "C370"

df_struct <- readRDS(here("data_processed", "ts_hourly_all_teams_features.rds")) %>%
  filter(team == team_no)

df_adj <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds")) %>%
  filter(team == team_no)

calls <- readRDS(here("data_processed", "calls_clean.rds"))

# Storebrand hourly volume to add back for raw (pre-removal) variant
storebrand_hourly <- calls %>%
  filter(
    call_responsible_department_l5 == team_no,
    common_customer_code == storebrand_code
  ) %>%
  mutate(ds = floor_date(call_start_time, unit = "hour")) %>%
  group_by(ds) %>%
  summarise(storebrand_y = sum(real_offered_call, na.rm = TRUE), .groups = "drop")

df_raw <- df_struct %>%
  left_join(storebrand_hourly, by = "ds") %>%
  mutate(
    storebrand_y = replace_na(storebrand_y, 0),
    y = y + storebrand_y
  ) %>%
  dplyr::select(-storebrand_y)

# ------------------------------------------------------------
# Train/test split (same as GLM evaluation)
# ------------------------------------------------------------
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
  test_end_dt <- max(df_struct$ds)
}

train_cutoff <- as.Date(test_start_dt)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
prepare_data <- function(df) {
  df %>%
    mutate(
      hour = factor(hour),
      weekday = factor(weekday, ordered = FALSE),
      month = factor(month),
      year_c = as.numeric(year) - 2024
    )
}

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

fit_and_eval <- function(df, variant) {
  tr <- df %>% filter(ds < train_cutoff) %>% prepare_data()
  te <- df %>% filter(ds >= test_start_dt, ds <= test_end_dt) %>% prepare_data()
  
  if (nrow(tr) == 0 || nrow(te) == 0) {
    return(NULL)
  }
  
  form <- y ~ hour + weekday + month + year_c +
    Juleferie + Vinterferie + Påskeferie + Sommerferie + Efterårsferie
  
  mod <- glm.nb(formula = form, data = tr)
  te$y_hat <- predict(mod, newdata = te, type = "response")
  
  compute_metrics(te) %>% mutate(variant = variant)
}

metrics_raw <- fit_and_eval(df_raw, "before_storebrand_removal")
metrics_removed <- fit_and_eval(df_struct, "after_storebrand_removal")
metrics_adjusted <- fit_and_eval(df_adj, "after_level_adjustment")

metrics_all <- bind_rows(metrics_raw, metrics_removed, metrics_adjusted)

readr::write_csv(
  metrics_all,
  file.path(out_dir, "team_no_bias_three_variants.csv")
)

# ------------------------------------------------------------
# Plot: Bias mean comparison
# ------------------------------------------------------------
plot_df <- metrics_all %>%
  dplyr::select(variant, Bias_mean) %>%
  mutate(
    variant = factor(
      variant,
      levels = c("before_storebrand_removal", "after_storebrand_removal", "after_level_adjustment"),
      labels = c("Før Storebrand-fjernelse", "Efter Storebrand-fjernelse", "Efter niveauskalering")
    )
  )

p_bias <- ggplot(plot_df, aes(variant, Bias_mean, fill = variant)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Bias_mean, 2)), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("#3E3E3E", "#2A6F97", "#D93945")) +
  labs(
    title = "Team NO - bias (gennemsnit) før/efter justeringer",
    x = "",
    y = "Bias (gennemsnit)",
    fill = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "team_no_bias_three_variants.png"),
  p_bias,
  width = 8,
  height = 4.5,
  dpi = 300
)

message("Team NO three-variant bias summary saved to: ", out_dir)
