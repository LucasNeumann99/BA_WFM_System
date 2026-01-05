#!/usr/bin/env Rscript
# ============================================================
# 25_xgb_residual_poc_compare.R
# ------------------------------------------------------------
# POC: XGBoost residuals on top of GLM and Prophet (log1p).
# - Calendar/time features only (hour, weekday, month, week, year, holidays)
# - No lags/rolling features
# - SE handled via SE_total + split (Prophet path)
# Outputs: BA_WFM_Output/analysis_extra/xgb_residual_poc
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)
library(xgboost)
library(prophet)

source(here("model_functions", "paths.R"))
source(here("model_functions", "se_total.R"))

set.seed(42)

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "xgb_residual_poc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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

holiday_regs <- c(
  "Juleferie", "Vinterferie", "Påskeferie", "Sommerferie", "Efterårsferie"
)

se1 <- "Team SE 1, Travelcare"
se2 <- "Team SE 2, Travelcare"
se_total <- "Team SE total"

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

align_xgb_matrix <- function(x, feature_names) {
  missing <- setdiff(feature_names, colnames(x))
  if (length(missing) > 0) {
    add <- matrix(0, nrow = nrow(x), ncol = length(missing))
    colnames(add) <- missing
    x <- cbind(x, add)
  }
  extra <- setdiff(colnames(x), feature_names)
  if (length(extra) > 0) {
    x <- x[, setdiff(colnames(x), extra), drop = FALSE]
  }
  x[, feature_names, drop = FALSE]
}

coerce_holidays <- function(df) {
  df %>% mutate(across(all_of(holiday_regs), ~ as.numeric(.x)))
}

xgb_params <- list(
  objective        = "reg:squarederror",
  eta              = 0.05,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8
)

# ------------------------------------------------------------
# GLM baseline + XGB residuals (per team)
# ------------------------------------------------------------
glm_models <- readRDS(here("models", "final_glm_negbin_by_team.rds"))

metrics_glm <- list()
metrics_glm_xgb <- list()

for (tm in names(glm_models)) {
  tr <- df_train_base %>% filter(team == tm)
  te <- df_test_base %>% filter(team == tm)
  if (nrow(tr) == 0 || nrow(te) == 0) next
  
  tr <- tr %>%
    mutate(
      hour = factor(hour),
      weekday = factor(weekday, ordered = FALSE),
      month = factor(month),
      year_c = as.numeric(year) - 2024
    ) %>%
    mutate(across(all_of(holiday_regs), ~ as.numeric(.x))) %>%
    droplevels()
  
  te <- te %>%
    mutate(
      hour = factor(hour, levels = levels(tr$hour)),
      weekday = factor(weekday, levels = levels(tr$weekday)),
      month = factor(month, levels = levels(tr$month)),
      year_c = as.numeric(year) - 2024
    ) %>%
    mutate(across(all_of(holiday_regs), ~ as.numeric(.x)))
  
  mod <- glm_models[[tm]]
  y_hat_glm_train <- predict(mod, newdata = tr, type = "response")
  y_hat_glm_test  <- predict(mod, newdata = te, type = "response")
  
  metrics_glm[[tm]] <- compute_metrics(te %>% mutate(y_hat = y_hat_glm_test)) %>%
    mutate(team = tm, model = "GLM")
  
  fe_train <- tr %>%
    transmute(
      hour,
      weekday,
      month,
      week,
      year,
      Juleferie,
      Vinterferie,
      Påskeferie,
      Sommerferie,
      Efterårsferie
    ) %>%
    coerce_holidays()
  
  X_train <- model.matrix(
    ~ hour + weekday + month + week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie - 1,
    data = fe_train
  )
  
  dtrain <- xgb.DMatrix(X_train, label = tr$y - y_hat_glm_train)
  xgb_fit <- xgb.train(
    params  = xgb_params,
    data    = dtrain,
    nrounds = 200,
    verbose = 0
  )
  
  fe_test <- te %>%
    transmute(
      hour,
      weekday,
      month,
      week,
      year,
      Juleferie,
      Vinterferie,
      Påskeferie,
      Sommerferie,
      Efterårsferie
    ) %>%
    coerce_holidays()
  
  X_test <- model.matrix(
    ~ hour + weekday + month + week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie - 1,
    data = fe_test
  )
  
  X_test <- align_xgb_matrix(X_test, colnames(X_train))
  resid_hat <- predict(xgb_fit, xgb.DMatrix(X_test))
  y_hat_glm_xgb <- pmax(0, y_hat_glm_test + resid_hat)
  
  metrics_glm_xgb[[tm]] <- compute_metrics(te %>% mutate(y_hat = y_hat_glm_xgb)) %>%
    mutate(team = tm, model = "GLM_XGB")
}

# ------------------------------------------------------------
# Prophet baseline + XGB residuals
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

metrics_prophet <- list()
metrics_prophet_xgb <- list()

teams <- unique(df_base$team)
teams_no_se <- setdiff(teams, c(se1, se2))

for (tm in teams_no_se) {
  tr <- df_train_base %>% filter(team == tm)
  te <- df_test_base %>% filter(team == tm)
  if (nrow(tr) == 0 || nrow(te) == 0) next
  
  train_p <- prep_prophet_df(tr)
  test_p  <- prep_prophet_df(te)
  
  mod <- fit_prophet_model(train_p)
  y_hat_prophet_train <- predict_prophet(mod, train_p)
  y_hat_prophet_test  <- predict_prophet(mod, test_p)
  
  metrics_prophet[[tm]] <- compute_metrics(te %>% mutate(y_hat = y_hat_prophet_test)) %>%
    mutate(team = tm, model = "Prophet")
  
  fe_train <- tr %>%
    transmute(
      hour,
      weekday,
      month,
      week,
      year,
      Juleferie,
      Vinterferie,
      Påskeferie,
      Sommerferie,
      Efterårsferie
    ) %>%
    coerce_holidays()
  
  X_train <- model.matrix(
    ~ hour + weekday + month + week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie - 1,
    data = fe_train
  )
  
  dtrain <- xgb.DMatrix(X_train, label = tr$y - y_hat_prophet_train)
  xgb_fit <- xgb.train(
    params  = xgb_params,
    data    = dtrain,
    nrounds = 200,
    verbose = 0
  )
  
  fe_test <- te %>%
    transmute(
      hour,
      weekday,
      month,
      week,
      year,
      Juleferie,
      Vinterferie,
      Påskeferie,
      Sommerferie,
      Efterårsferie
    ) %>%
    coerce_holidays()
  
  X_test <- model.matrix(
    ~ hour + weekday + month + week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie - 1,
    data = fe_test
  )
  
  X_test <- align_xgb_matrix(X_test, colnames(X_train))
  resid_hat <- predict(xgb_fit, xgb.DMatrix(X_test))
  y_hat_prophet_xgb <- pmax(0, y_hat_prophet_test + resid_hat)
  
  metrics_prophet_xgb[[tm]] <- compute_metrics(te %>% mutate(y_hat = y_hat_prophet_xgb)) %>%
    mutate(team = tm, model = "Prophet_XGB")
}

# SE total + split for Prophet path
if (all(c(se1, se2) %in% teams)) {
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
  y_hat_prophet_train <- predict_prophet(mod_se, train_p)
  y_hat_prophet_test  <- predict_prophet(mod_se, test_p)
  
  fe_train <- se_train %>%
    transmute(
      hour = lubridate::hour(ds),
      weekday = lubridate::wday(ds, label = TRUE, week_start = 1),
      month = lubridate::month(ds),
      week = lubridate::isoweek(ds),
      year = lubridate::year(ds),
      Juleferie,
      Vinterferie,
      Påskeferie,
      Sommerferie,
      Efterårsferie
    ) %>%
    coerce_holidays()
  
  X_train <- model.matrix(
    ~ hour + weekday + month + week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie - 1,
    data = fe_train
  )
  
  dtrain <- xgb.DMatrix(X_train, label = se_train$y - y_hat_prophet_train)
  xgb_fit <- xgb.train(
    params  = xgb_params,
    data    = dtrain,
    nrounds = 200,
    verbose = 0
  )
  
  fe_test <- se_test %>%
    transmute(
      hour = lubridate::hour(ds),
      weekday = lubridate::wday(ds, label = TRUE, week_start = 1),
      month = lubridate::month(ds),
      week = lubridate::isoweek(ds),
      year = lubridate::year(ds),
      Juleferie,
      Vinterferie,
      Påskeferie,
      Sommerferie,
      Efterårsferie
    ) %>%
    coerce_holidays()
  
  X_test <- model.matrix(
    ~ hour + weekday + month + week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie - 1,
    data = fe_test
  )
  
  X_test <- align_xgb_matrix(X_test, colnames(X_train))
  resid_hat <- predict(xgb_fit, xgb.DMatrix(X_test))
  y_hat_prophet_xgb <- pmax(0, y_hat_prophet_test + resid_hat)
  
  se1_te <- df_test_base %>% filter(team == se1)
  se2_te <- df_test_base %>% filter(team == se2)
  
  metrics_prophet[[se1]] <- compute_metrics(se1_te %>% mutate(y_hat = y_hat_prophet_test * p_se1)) %>%
    mutate(team = se1, model = "Prophet")
  metrics_prophet[[se2]] <- compute_metrics(se2_te %>% mutate(y_hat = y_hat_prophet_test * p_se2)) %>%
    mutate(team = se2, model = "Prophet")
  
  metrics_prophet_xgb[[se1]] <- compute_metrics(se1_te %>% mutate(y_hat = y_hat_prophet_xgb * p_se1)) %>%
    mutate(team = se1, model = "Prophet_XGB")
  metrics_prophet_xgb[[se2]] <- compute_metrics(se2_te %>% mutate(y_hat = y_hat_prophet_xgb * p_se2)) %>%
    mutate(team = se2, model = "Prophet_XGB")
}

# ------------------------------------------------------------
# Combine + compare
# ------------------------------------------------------------
metrics_all <- bind_rows(
  bind_rows(metrics_glm),
  bind_rows(metrics_glm_xgb),
  bind_rows(metrics_prophet),
  bind_rows(metrics_prophet_xgb)
)

readr::write_csv(metrics_all, file.path(out_dir, "metrics_xgb_residual_poc.csv"))

delta <- metrics_all %>%
  filter(model %in% c("GLM", "GLM_XGB", "Prophet", "Prophet_XGB")) %>%
  select(team, model, RMSE, Bias_mean) %>%
  pivot_wider(names_from = model, values_from = c(RMSE, Bias_mean)) %>%
  mutate(
    delta_rmse_glm = RMSE_GLM_XGB - RMSE_GLM,
    delta_bias_glm = Bias_mean_GLM_XGB - Bias_mean_GLM,
    delta_rmse_prophet = RMSE_Prophet_XGB - RMSE_Prophet,
    delta_bias_prophet = Bias_mean_Prophet_XGB - Bias_mean_Prophet
  )

readr::write_csv(delta, file.path(out_dir, "metrics_xgb_residual_delta.csv"))

message("XGB residual POC saved to: ", out_dir)
