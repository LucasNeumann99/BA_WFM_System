# ============================================================
# 19_operational_forecast_v2.R
# ------------------------------------------------------------
# Formål: lav operationelle forecasts for angivet horisont (GLM + XGB-residual).
# Output: <results_base>/v2/operational/fc_operational_raw_v2.rds (y_hat_raw, model_used, is_recursive).
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(MASS)
library(jsonlite)
library(xgboost)

source(here("model_functions", "paths.R"))

cfg <- fromJSON(here("config", "forecast_v2.json"))
tz_info <- cfg$timezone %||% "UTC"
op_cfg  <- cfg$operational

forecast_start <- ymd_hms(op_cfg$forecast_start, tz = tz_info)
forecast_end   <- ymd_hms(op_cfg$forecast_end,   tz = tz_info)

paths <- get_pipeline_paths()
op_dir <- file.path(paths$results, "v2", "operational")
dir.create(op_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(op_dir, "fc_operational_raw_v2.rds")

# ------------------------------------------------------------
# Data
# ------------------------------------------------------------
df_base <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))
df_future <- readRDS(here("data_processed", "ts_future_all_teams_features.rds"))

xgb_resid_by_team <- readRDS(here("models", "final_xgb_residual_by_team.rds"))

# ------------------------------------------------------------
# Helper: train baseline model
# ------------------------------------------------------------
train_baseline <- function(team, train_df) {
  form <- y ~ hour + weekday + month +
    year_c +
    Juleferie + Vinterferie + Påskeferie +
    Sommerferie + Efterårsferie
  
  train_df <- train_df %>%
    mutate(
      hour    = factor(hour),
      weekday = factor(weekday, ordered = FALSE),
      month   = factor(month),
      year_c  = as.numeric(year) - 2024
    ) %>%
    droplevels()
  
  glm.nb(formula = form, data = train_df)
}

# ------------------------------------------------------------
# Helper: align XGBoost feature matrix
# ------------------------------------------------------------
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

load_xgb_model <- function(entry) {
  if (is.null(entry$model_raw)) return(NULL)
  xgb.unserialize(entry$model_raw)
}

# ------------------------------------------------------------
# Helper: direct forecast for baseline-team
# ------------------------------------------------------------
forecast_baseline <- function(team, model, horizon_df, train_levels) {
  horizon_df %>%
    mutate(
      hour    = factor(hour,    levels = train_levels$hour),
      weekday = factor(weekday, levels = train_levels$weekday),
      month   = factor(month,   levels = train_levels$month),
      year_c  = as.numeric(year) - 2024
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
  hist_df <- df_base %>%
    filter(team == tm, ds < forecast_start) %>%
    mutate(
      hour    = factor(hour),
      weekday = factor(weekday, ordered = FALSE),
      month   = factor(month),
      year_c  = as.numeric(year) - 2024
    ) %>%
    droplevels()
  
  horizon_df <- df_future %>%
    filter(team == tm, ds >= forecast_start, ds <= forecast_end)
  
  if (nrow(hist_df) == 0 || nrow(horizon_df) == 0) next
  
  model <- train_baseline(tm, hist_df)
  levels_list <- list(
    hour    = levels(hist_df$hour),
    weekday = levels(hist_df$weekday),
    month   = levels(hist_df$month)
  )
  fc_tm <- forecast_baseline(tm, model, horizon_df, levels_list)
  
  xgb_entry <- xgb_resid_by_team[[tm]]
  if (!is.null(xgb_entry)) {
    num_vars_other <- c(
      "Juleferie", "Vinterferie", "Påskeferie",
      "Sommerferie", "Efterårsferie"
    )
    fe_op <- horizon_df %>%
      mutate(
        weekday = factor(weekday),
        month   = factor(month)
      ) %>%
      mutate(across(all_of(num_vars_other), as.numeric)) %>%
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
      )
    
    X_op <- model.matrix(
      ~ hour + weekday + month + week + year +
        Juleferie + Vinterferie + Påskeferie +
        Sommerferie + Efterårsferie - 1,
      data = fe_op
    )
    
    X_op <- align_xgb_matrix(X_op, xgb_entry$feature_names)
    d_op <- xgb.DMatrix(X_op)
    xgb_model <- load_xgb_model(xgb_entry)
    resid_hat <- predict(xgb_model, d_op)
    
    fc_tm <- fc_tm %>%
      mutate(
        y_hat_raw = pmax(0, y_hat_raw + resid_hat),
        model_used = "GLM_NegBin_xgb_resid"
      )
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
