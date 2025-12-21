# ============================================================
# 18_backtest_rolling_final_glm.R
# ------------------------------------------------------------
# Formål: kør rolling-origin backtests for final GLM (lags/baseline) uden volumen-chok.
# Gemmer forecasts/metrics i v2-mapper. Ingen scenarier anvendes her.
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(MASS)
library(jsonlite)

source(here("model_functions", "paths.R"))

# ------------------------------------------------------------
# Paths og config
# ------------------------------------------------------------
cfg_path   <- here("config", "forecast_v2.json")
cfg        <- fromJSON(cfg_path)

backtest_cfg <- cfg$backtest
tz_info      <- cfg$timezone %||% "UTC"

paths <- get_pipeline_paths()
backtest_dir <- file.path(paths$results, "v2", "backtests")
diag_dir <- file.path(paths$output, "v2", "diagnostics")
dir.create(backtest_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

fc_out_path     <- file.path(backtest_dir, "fc_backtest_rolling.rds")
metrics_out_path<- file.path(diag_dir, "metrics_backtest_rolling.csv")

# ------------------------------------------------------------
# Data og teams
# ------------------------------------------------------------
df_base <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))
df_lags <- readRDS(here("data_processed", "ts_hourly_all_teams_lags.rds"))

# Kør baseline på alle teams (ingen lag i backtest)
teams_use_lags <- character(0)

# ------------------------------------------------------------
# Helper: beregn rolling fold-datoer
# ------------------------------------------------------------
build_folds <- function(ds_min, ds_max, n_folds, test_window_days, gap_days) {
  folds <- list()
  for (k in seq_len(n_folds)) {
    test_end   <- ds_max - days((k - 1) * test_window_days)
    test_start <- test_end - days(test_window_days) + hours(1)
    train_end  <- test_start - days(gap_days) - hours(1)
    train_start<- ds_min
    folds[[k]] <- list(
      fold_id    = k,
      train_start= train_start,
      train_end  = train_end,
      test_start = test_start,
      test_end   = test_end
    )
  }
  folds
}

# ------------------------------------------------------------
# Helper: train/predict for ét team/fold
# ------------------------------------------------------------
train_and_forecast <- function(team, fold, use_lags) {
  if (use_lags) {
    dtrain <- df_lags %>%
      filter(team == !!team,
             ds >= fold$train_start,
             ds <= fold$train_end) %>%
      drop_na(lag_1, lag_24, lag_48, lag_168,
              roll_mean_24, roll_mean_72, roll_mean_168)
    
    dtest <- df_lags %>%
      filter(team == !!team,
             ds >= fold$test_start,
             ds <= fold$test_end) %>%
      drop_na(lag_1, lag_24, lag_48, lag_168,
              roll_mean_24, roll_mean_72, roll_mean_168)
    
    if (nrow(dtrain) == 0 || nrow(dtest) == 0) return(NULL)
    
    dtrain <- dtrain %>%
      mutate(
        weekday = factor(weekday),
        month   = factor(month)
      )
    dtest <- dtest %>%
      mutate(
        weekday = factor(weekday, levels = levels(dtrain$weekday)),
        month   = factor(month,   levels = levels(dtrain$month))
      )
    
    form <- y ~ hour + weekday + month +
      week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie +
      lag_1 + lag_24 + lag_48 + lag_168 +
      roll_mean_24 + roll_mean_72 + roll_mean_168
    
    mod <- glm.nb(formula = form, data = dtrain)
    
    preds <- predict(mod, newdata = dtest, type = "response")
    
    tibble(
      team        = team,
      ds          = dtest$ds,
      y           = dtest$y,
      y_hat_raw   = preds,
      model_used  = "GLM_NegBin_lags",
      fold_id     = fold$fold_id,
      is_recursive= TRUE
    )
  } else {
    dtrain <- df_base %>%
      filter(team == !!team,
             ds >= fold$train_start,
             ds <= fold$train_end)
    dtest <- df_base %>%
      filter(team == !!team,
             ds >= fold$test_start,
             ds <= fold$test_end)
    
    if (nrow(dtrain) == 0 || nrow(dtest) == 0) return(NULL)
    
    dtrain <- dtrain %>%
      mutate(
        hour    = factor(hour),
        weekday = factor(weekday, ordered = FALSE),
        month   = factor(month),
        year_c  = as.numeric(year) - 2024
      ) %>%
      droplevels()
    
    dtest <- dtest %>%
      mutate(
        hour    = factor(hour,    levels = levels(dtrain$hour)),
        weekday = factor(weekday, levels = levels(dtrain$weekday)),
        month   = factor(month,   levels = levels(dtrain$month)),
        year_c  = as.numeric(year) - 2024
      )
    
    form <- y ~ hour + weekday + month +
      year_c +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie
    
    mod <- glm.nb(formula = form, data = dtrain)
    preds <- predict(mod, newdata = dtest, type = "response")
    
    tibble(
      team        = team,
      ds          = dtest$ds,
      y           = dtest$y,
      y_hat_raw   = preds,
      model_used  = "GLM_NegBin_baseline",
      fold_id     = fold$fold_id,
      is_recursive= FALSE
    )
  }
}

# ------------------------------------------------------------
# Run folds
# ------------------------------------------------------------
ds_min <- min(df_base$ds, na.rm = TRUE)
ds_max <- max(df_base$ds[df_base$y >= 0], na.rm = TRUE)

folds <- build_folds(
  ds_min = ds_min,
  ds_max = ds_max,
  n_folds = backtest_cfg$n_folds,
  test_window_days = backtest_cfg$test_window_days,
  gap_days = backtest_cfg$gap_days
)

all_fc <- list()

for (tm in unique(df_base$team)) {
  use_lags <- tm %in% teams_use_lags
  for (fd in folds) {
    res <- train_and_forecast(tm, fd, use_lags)
    if (!is.null(res)) all_fc[[length(all_fc) + 1]] <- res
  }
}

fc_backtest <- bind_rows(all_fc)
saveRDS(fc_backtest, fc_out_path)

# ------------------------------------------------------------
# Metrics
# ------------------------------------------------------------
metrics <- fc_backtest %>%
  group_by(team, fold_id) %>%
  summarise(
    n       = sum(!is.na(y) & !is.na(y_hat_raw)),
    RMSE    = sqrt(mean((y - y_hat_raw)^2, na.rm = TRUE)),
    MAE     = mean(abs(y - y_hat_raw), na.rm = TRUE),
    MAPE    = mean(abs(y - y_hat_raw) / pmax(y, 1), na.rm = TRUE),
    SMAPE   = mean(abs(y - y_hat_raw) / ((abs(y) + abs(y_hat_raw)) / 2), na.rm = TRUE),
    Bias_mean = mean(y_hat_raw - y, na.rm = TRUE),
    Bias_sd   = sd(y_hat_raw - y, na.rm = TRUE),
    model_used = first(model_used),
    .groups = "drop"
  )

write_csv(metrics, metrics_out_path)

message("✔ Backtest færdig. Forecasts: ", fc_out_path)
message("✔ Metrics: ", metrics_out_path)
