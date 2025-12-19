# ============================================================
# 17_operational_forecast_v2.R
# ------------------------------------------------------------
# Formål: lav operationelle forecasts (rå model-output) for angivet horisont.
# Lag-teams køres rekursivt time-for-time; baseline-teams uses direkte feature-prediktion.
# Output: results/v2/operational/fc_operational_raw_v2.rds (y_hat_raw, model_used, is_recursive).
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(MASS)
library(jsonlite)

cfg <- fromJSON(here("config", "forecast_v2.json"))
tz_info <- cfg$timezone %||% "UTC"
op_cfg  <- cfg$operational

forecast_start <- ymd_hms(op_cfg$forecast_start, tz = tz_info)
forecast_end   <- ymd_hms(op_cfg$forecast_end,   tz = tz_info)

dir.create(here("results", "v2", "operational"), recursive = TRUE, showWarnings = FALSE)
out_path <- here("results", "v2", "operational", "fc_operational_raw_v2.rds")

# ------------------------------------------------------------
# Data
# ------------------------------------------------------------
df_base <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))
df_lags <- readRDS(here("data_processed", "ts_hourly_all_teams_lags.rds"))

teams_use_lags <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare"
)

# ------------------------------------------------------------
# Helper: train baseline model
# ------------------------------------------------------------
train_baseline <- function(team, train_df) {
  form <- y ~ hour + weekday + month +
    week + year +
    Juleferie + Vinterferie + Påskeferie +
    Sommerferie + Efterårsferie
  
  train_df <- train_df %>%
    mutate(
      hour    = factor(hour),
      weekday = factor(weekday, ordered = FALSE),
      month   = factor(month)
    ) %>%
    droplevels()
  
  glm.nb(formula = form, data = train_df)
}

# ------------------------------------------------------------
# Helper: train lag model
# ------------------------------------------------------------
train_lag <- function(team, train_df) {
  form <- y ~ hour + weekday + month +
    week + year +
    Juleferie + Vinterferie + Påskeferie +
    Sommerferie + Efterårsferie +
    lag_1 + lag_24 + lag_48 + lag_168 +
    roll_mean_24 + roll_mean_72 + roll_mean_168
  
  train_df <- train_df %>%
    drop_na(lag_1, lag_24, lag_48, lag_168,
            roll_mean_24, roll_mean_72, roll_mean_168) %>%
    mutate(
      weekday = factor(weekday),
      month   = factor(month)
    )
  
  glm.nb(formula = form, data = train_df)
}

# ------------------------------------------------------------
# Helper: recursive forecast for lag-team
# ------------------------------------------------------------
forecast_recursive_lag <- function(team, model, history_df, horizon_df) {
  # history_df: full past with y (actuals) up to forecast_start
  # horizon_df: rows with ds in forecast window, with calendar features
  history_vec <- history_df %>%
    arrange(ds) %>%
    pull(y)
  
  res_list <- list()
  
  # ensure factors align
  horizon_df <- horizon_df %>%
    mutate(
      weekday = factor(weekday, levels = levels(history_df$weekday)),
      month   = factor(month,   levels = levels(history_df$month))
    )
  
  for (i in seq_len(nrow(horizon_df))) {
    row <- horizon_df[i, ]
    # compute lags/rolls fra history_vec
    lag_1   <- if (length(history_vec) >= 1)  tail(history_vec, 1)   else NA_real_
    lag_24  <- if (length(history_vec) >= 24) tail(history_vec, 24)[1] else NA_real_
    lag_48  <- if (length(history_vec) >= 48) tail(history_vec, 48)[1] else NA_real_
    lag_168 <- if (length(history_vec) >= 168)tail(history_vec, 168)[1] else NA_real_
    
    roll_mean_24  <- if (length(history_vec) >= 24)  mean(tail(history_vec, 24))  else NA_real_
    roll_mean_72  <- if (length(history_vec) >= 72)  mean(tail(history_vec, 72))  else NA_real_
    roll_mean_168 <- if (length(history_vec) >= 168) mean(tail(history_vec, 168)) else NA_real_
    
    newdata <- row %>%
      mutate(
        lag_1 = lag_1,
        lag_24 = lag_24,
        lag_48 = lag_48,
        lag_168 = lag_168,
        roll_mean_24 = roll_mean_24,
        roll_mean_72 = roll_mean_72,
        roll_mean_168 = roll_mean_168
      )
    
    y_hat <- predict(model, newdata = newdata, type = "response")
    history_vec <- c(history_vec, y_hat)
    
    res_list[[i]] <- tibble(
      team        = team,
      ds          = row$ds,
      y_hat_raw   = as.numeric(y_hat),
      model_used  = "GLM_NegBin_lags",
      is_recursive= TRUE
    )
  }
  
  bind_rows(res_list)
}

# ------------------------------------------------------------
# Helper: direct forecast for baseline-team
# ------------------------------------------------------------
forecast_baseline <- function(team, model, horizon_df, train_levels) {
  horizon_df %>%
    mutate(
      hour    = factor(hour,    levels = train_levels$hour),
      weekday = factor(weekday, levels = train_levels$weekday),
      month   = factor(month,   levels = train_levels$month)
    ) %>%
    transmute(
      team = team,
      ds   = ds,
      y_hat_raw = predict(model, newdata = ., type = "response"),
      model_used = "GLM_NegBin_baseline",
      is_recursive = FALSE
    )
}

# ------------------------------------------------------------
# Kør for alle teams
# ------------------------------------------------------------
fc_list <- list()
run_id_val <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = tz_info)
gen_ts <- Sys.time()

for (tm in unique(df_base$team)) {
  use_lags <- tm %in% teams_use_lags
  
  if (use_lags) {
    hist_df <- df_lags %>%
      filter(team == tm, ds < forecast_start) %>%
      mutate(
        weekday = factor(weekday),
        month   = factor(month)
      ) %>%
      drop_na(lag_1, lag_24, lag_48, lag_168,
              roll_mean_24, roll_mean_72, roll_mean_168)
    
    horizon_df <- df_base %>%
      filter(team == tm, ds >= forecast_start, ds <= forecast_end)
    
    if (nrow(hist_df) == 0 || nrow(horizon_df) == 0) next
    
    model <- train_lag(tm, hist_df)
    fc_tm <- forecast_recursive_lag(tm, model, hist_df, horizon_df)
  } else {
    hist_df <- df_base %>%
      filter(team == tm, ds < forecast_start) %>%
      mutate(
        hour    = factor(hour),
        weekday = factor(weekday, ordered = FALSE),
        month   = factor(month)
      ) %>%
      droplevels()
    
    horizon_df <- df_base %>%
      filter(team == tm, ds >= forecast_start, ds <= forecast_end)
    
    if (nrow(hist_df) == 0 || nrow(horizon_df) == 0) next
    
    model <- train_baseline(tm, hist_df)
    levels_list <- list(
      hour    = levels(hist_df$hour),
      weekday = levels(hist_df$weekday),
      month   = levels(hist_df$month)
    )
    fc_tm <- forecast_baseline(tm, model, horizon_df, levels_list)
  }
  
  fc_tm <- fc_tm %>%
    mutate(
      generated_at = gen_ts,
      run_id       = run_id_val
    )
  
  fc_list[[length(fc_list) + 1]] <- fc_tm
}

fc_operational <- bind_rows(fc_list)
saveRDS(fc_operational, out_path)

message("✔ Operational rå forecast gemt: ", out_path)
