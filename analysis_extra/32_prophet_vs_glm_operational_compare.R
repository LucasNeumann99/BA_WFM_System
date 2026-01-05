#!/usr/bin/env Rscript
# ============================================================
# 32_prophet_vs_glm_operational_compare.R
# ------------------------------------------------------------
# Compare GLM vs Prophet in operational scenario context:
# - Apply volume shocks to Prophet POC forecast (same rules as GLM scenarios)
# - Compare RMSE/Bias on historical window with actuals
# - Summarize scenario volume deltas
# Output: BA_WFM_Output/analysis_extra/prophet_poc/compare
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))
source(here("model_functions", "se_total.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "prophet_poc", "compare")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Load GLM scenario forecast + Prophet POC forecast
# ------------------------------------------------------------
glm_path <- file.path(paths$results, "v2", "scenarios", "fc_operational_scenario_v2.rds")
if (!file.exists(glm_path)) {
  stop("Missing GLM scenario forecast: ", glm_path)
}
glm <- readRDS(glm_path)

prophet_path <- file.path(paths$output, "analysis_extra", "prophet_poc", "fc_prophet_poc.rds")
if (!file.exists(prophet_path)) {
  stop("Missing Prophet POC forecast: ", prophet_path)
}
prophet <- readRDS(prophet_path)

# ------------------------------------------------------------
# Apply volume shocks to Prophet (same as step 18)
# ------------------------------------------------------------
shocks_path <- here("config", "volume_shocks.csv")
shocks <- readr::read_csv(shocks_path, show_col_types = FALSE) %>%
  mutate(
    start_date = as_datetime(start_date),
    end_date   = if_else(is.na(end_date) | end_date == "", NA, as_datetime(end_date)),
    priority   = if_else(is.na(priority), 0L, priority)
  ) %>%
  arrange(desc(priority))

prophet <- prophet %>%
  mutate(
    y_hat = y_hat,
    scenario_label = NA_character_,
    shock_multiplier_applied = 1
  )

apply_shock <- function(fc_df, shock_row) {
  idx <- fc_df$team == shock_row$team &
    fc_df$ds >= shock_row$start_date &
    (is.na(shock_row$end_date) | fc_df$ds < shock_row$end_date) &
    is.na(fc_df$scenario_label)
  
  fc_df$scenario_label[idx] <- shock_row$label
  fc_df$shock_multiplier_applied[idx] <- shock_row$multiplier
  fc_df$y_hat[idx] <- fc_df$y_hat[idx] * shock_row$multiplier
  fc_df
}

if (!all(c("p_se1", "p_se2") %in% names(prophet))) {
  prophet <- ensure_se_share_cols(prophet, 0.5, 0.5)
}

for (i in seq_len(nrow(shocks))) {
  shock <- shocks[i, ]
  if (shock$team == "Team SE total") {
    idx_total <- prophet$team == "Team SE total" &
      prophet$ds >= shock$start_date &
      (is.na(shock$end_date) | prophet$ds < shock$end_date) &
      is.na(prophet$scenario_label)
    
    if (any(idx_total)) {
      total_shocked <- prophet$y_hat[idx_total] * shock$multiplier
      prophet$y_hat[idx_total] <- total_shocked
      prophet$scenario_label[idx_total] <- shock$label
      prophet$shock_multiplier_applied[idx_total] <- shock$multiplier
      
      idx_se <- prophet$team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare") &
        prophet$ds %in% prophet$ds[idx_total] &
        is.na(prophet$scenario_label)
      
      prophet$y_hat[idx_se & prophet$team == "Team SE 1, Travelcare"] <-
        total_shocked[match(prophet$ds[idx_se & prophet$team == "Team SE 1, Travelcare"], prophet$ds[idx_total])] *
        prophet$p_se1[idx_se & prophet$team == "Team SE 1, Travelcare"]
      
      prophet$y_hat[idx_se & prophet$team == "Team SE 2, Travelcare"] <-
        total_shocked[match(prophet$ds[idx_se & prophet$team == "Team SE 2, Travelcare"], prophet$ds[idx_total])] *
        prophet$p_se2[idx_se & prophet$team == "Team SE 2, Travelcare"]
      
      prophet$scenario_label[idx_se] <- shock$label
      prophet$shock_multiplier_applied[idx_se] <- shock$multiplier
    }
  } else {
    prophet <- apply_shock(prophet, shock)
  }
}

prophet <- prophet %>% mutate(model_used = "Prophet_scenario")
glm <- glm %>% mutate(model_used = "GLM_scenario")

# ------------------------------------------------------------
# Metrics (only where y exists in Prophet POC)
# ------------------------------------------------------------
compute_metrics <- function(df) {
  n_obs <- sum(!is.na(df$y_hat) & !is.na(df$y))
  if (n_obs == 0) {
    return(tibble(
      n = 0,
      RMSE = NA_real_,
      MAE = NA_real_,
      MAPE = NA_real_,
      SMAPE = NA_real_,
      Bias_mean = NA_real_,
      Bias_sd = NA_real_
    ))
  }
  df %>%
    summarise(
      n = n_obs,
      RMSE = sqrt(mean((y - y_hat)^2, na.rm = TRUE)),
      MAE  = mean(abs(y - y_hat), na.rm = TRUE),
      MAPE = mean(abs(y - y_hat) / pmax(y, 1), na.rm = TRUE),
      SMAPE = mean(abs(y - y_hat) / ((abs(y) + abs(y_hat)) / 2), na.rm = TRUE),
      Bias_mean = mean(y_hat - y, na.rm = TRUE),
      Bias_sd   = sd(y_hat - y, na.rm = TRUE)
    )
}

# Attach actuals to GLM scenarios (operational file has no y)
actuals <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds")) %>%
  select(team, ds, y)

glm <- glm %>%
  left_join(actuals, by = c("team", "ds"))

prophet <- prophet %>%
  left_join(actuals, by = c("team", "ds"), suffix = c("", "_actual")) %>%
  mutate(y = coalesce(y, y_actual)) %>%
  select(-y_actual)

prophet_metrics <- prophet %>%
  group_by(team) %>%
  group_modify(~ compute_metrics(.x)) %>%
  ungroup() %>%
  mutate(model = "Prophet_scenario")

glm_metrics <- glm %>%
  group_by(team) %>%
  group_modify(~ compute_metrics(.x)) %>%
  ungroup() %>%
  mutate(model = "GLM_scenario")

metrics <- bind_rows(glm_metrics, prophet_metrics)
readr::write_csv(metrics, file.path(out_dir, "metrics_glm_vs_prophet_scenarios.csv"))

# ------------------------------------------------------------
# Scenario volume deltas (sum y_hat)
# ------------------------------------------------------------
vol_glm <- glm %>%
  group_by(team, scenario_label) %>%
  summarise(volume = sum(y_hat, na.rm = TRUE), .groups = "drop") %>%
  mutate(model = "GLM_scenario")

vol_prophet <- prophet %>%
  group_by(team, scenario_label) %>%
  summarise(volume = sum(y_hat, na.rm = TRUE), .groups = "drop") %>%
  mutate(model = "Prophet_scenario")

volumes <- bind_rows(vol_glm, vol_prophet)
readr::write_csv(volumes, file.path(out_dir, "volume_glm_vs_prophet_scenarios.csv"))

message("Saved scenario comparison to: ", out_dir)
