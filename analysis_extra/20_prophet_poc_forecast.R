#!/usr/bin/env Rscript
# ============================================================
# 20_prophet_poc_forecast.R
# ------------------------------------------------------------
# Proof-of-concept Prophet baseline (log1p) under GLM conditions:
# - Same train/test window as config/forecast_v2.json
# - Uses adjusted series (Team NO level shift already applied)
# - SE modeled as SE_total and split by actuals-share (6m rolling)
# - Saves diagnostics + metrics to output/analysis_extra/prophet_poc
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)
library(prophet)

source(here("model_functions", "paths.R"))
source(here("model_functions", "se_total.R"))

# ------------------------------------------------------------
# Config + paths
# ------------------------------------------------------------
paths <- get_pipeline_paths()
out_base <- file.path(paths$output, "analysis_extra", "prophet_poc")
dir.create(out_base, recursive = TRUE, showWarnings = FALSE)

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

train_cutoff <- as.Date(test_start_dt)

df_train_base <- df_base %>% filter(ds < train_cutoff)
df_test_base  <- df_base %>% filter(ds >= test_start_dt, ds <= test_end_dt)

se1 <- "Team SE 1, Travelcare"
se2 <- "Team SE 2, Travelcare"
se_total <- "Team SE total"

holiday_regs <- c(
  "Juleferie", "Vinterferie", "Påskeferie", "Sommerferie", "Efterårsferie"
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
prep_prophet_df <- function(df) {
  df %>%
    transmute(
      ds = ds,
      y = log1p(y),
      across(all_of(holiday_regs), ~ as.numeric(.x))
    )
}

fit_prophet_model <- function(train_df) {
  m <- prophet(
    weekly.seasonality = TRUE,
    daily.seasonality = TRUE,
    yearly.seasonality = TRUE,
    seasonality.mode = "additive",
    changepoint.prior.scale = 0.05,
    fit = FALSE
  )
  
  for (r in holiday_regs) {
    m <- add_regressor(m, r)
  }
  
  fit.prophet(m, train_df)
}

predict_prophet <- function(model, df_future) {
  preds <- predict(model, df_future)
  pmax(0, exp(preds$yhat) - 1)
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

plot_colors <- c(
  "Actual" = "#3E3E3E",
  "Prophet" = "#D93945"
)

plot_forecast_vs_actual <- function(df, tm) {
  ggplot(df, aes(ds)) +
    geom_line(aes(y = y, colour = "Actual")) +
    geom_line(aes(y = y_hat, colour = "Prophet")) +
    scale_colour_manual(values = plot_colors) +
    theme_minimal() +
    labs(
      title = paste("Prophet POC Forecast -", tm),
      y = "Calls",
      colour = ""
    )
}

plot_residuals <- function(df, tm) {
  ggplot(df %>% mutate(resid = y - y_hat), aes(ds, resid)) +
    geom_line(color = plot_colors[["Actual"]]) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = paste("Residuals -", tm),
      y = "Residual"
    )
}

# ------------------------------------------------------------
# Train + predict
# ------------------------------------------------------------
teams <- unique(df_base$team)
teams_no_se <- setdiff(teams, c(se1, se2))

fc_list <- list()
models_out <- list()

for (tm in teams_no_se) {
  message("---- Prophet POC: ", tm)
  tr <- df_train_base %>% filter(team == tm)
  te <- df_test_base %>% filter(team == tm)
  
  if (nrow(tr) == 0 || nrow(te) == 0) {
    warning("No data for training/forecast for ", tm, ". Skipping.")
    next
  }
  
  train_p <- prep_prophet_df(tr)
  test_p  <- prep_prophet_df(te)
  
  mod <- fit_prophet_model(train_p)
  y_hat <- predict_prophet(mod, test_p)
  
  models_out[[tm]] <- mod
  fc_list[[tm]] <- te %>%
    mutate(
      y_hat = y_hat,
      model_used = "Prophet_baseline"
    )
}

# ------------------------------------------------------------
# SE total model + split
# ------------------------------------------------------------
if (all(c(se1, se2) %in% teams)) {
  message("---- Prophet POC: ", se_total)
  
  se_train <- df_train_base %>%
    filter(team %in% c(se1, se2)) %>%
    group_by(ds) %>%
    summarise(
      y = sum(y, na.rm = TRUE),
      across(all_of(holiday_regs), ~ first(.x)),
      .groups = "drop"
    )
  
  se_test <- df_test_base %>%
    filter(team %in% c(se1, se2)) %>%
    group_by(ds) %>%
    summarise(
      y = sum(y, na.rm = TRUE),
      across(all_of(holiday_regs), ~ first(.x)),
      .groups = "drop"
    )
  
  p <- se_actual_share(df_base, cutoff_ds = test_start_dt, n_months = 6)
  p_se1 <- p$p_se1
  p_se2 <- p$p_se2
  
  train_p <- prep_prophet_df(se_train)
  test_p  <- prep_prophet_df(se_test)
  
  mod_se <- fit_prophet_model(train_p)
  y_hat_total <- predict_prophet(mod_se, test_p)
  
  models_out[[se_total]] <- mod_se
  
  fc_list[[se_total]] <- se_test %>%
    mutate(
      team = se_total,
      y_hat = y_hat_total,
      model_used = "Prophet_SE_total"
    )
  
  se1_te <- df_test_base %>% filter(team == se1)
  se2_te <- df_test_base %>% filter(team == se2)
  
  fc_list[[se1]] <- se1_te %>%
    mutate(
      y_hat = y_hat_total * p_se1,
      model_used = "Prophet_SE_total_split"
    )
  
  fc_list[[se2]] <- se2_te %>%
    mutate(
      y_hat = y_hat_total * p_se2,
      model_used = "Prophet_SE_total_split"
    )
}

fc_all <- bind_rows(fc_list)

# ------------------------------------------------------------
# Metrics + plots
# ------------------------------------------------------------
metrics_all <- list()
for (tm in unique(fc_all$team)) {
  df <- fc_all %>% filter(team == tm)
  
  diag_dir <- file.path(out_base, tm, "diagnostics")
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  
  ggsave(
    file.path(diag_dir, "forecast_vs_actual.png"),
    plot = plot_forecast_vs_actual(df, tm),
    width = 10, height = 5, dpi = 300
  )
  
  ggsave(
    file.path(diag_dir, "residuals.png"),
    plot = plot_residuals(df, tm),
    width = 10, height = 5, dpi = 300
  )
  
  m <- compute_metrics(df) %>%
    mutate(team = tm, model = unique(df$model_used))
  
  readr::write_csv(
    m,
    file.path(diag_dir, "metrics_prophet_poc.csv")
  )
  
  metrics_all[[tm]] <- m
}

metrics_final <- bind_rows(metrics_all)
readr::write_csv(
  metrics_final,
  file.path(out_base, "metrics_prophet_poc.csv")
)

saveRDS(
  models_out,
  here("models", "prophet_poc_by_team.rds")
)

saveRDS(
  fc_all,
  file.path(out_base, "fc_prophet_poc.rds")
)

  message("Prophet POC complete: ", out_base)
