# ============================================================
# 12_train_glm_lags.R
# ------------------------------------------------------------
# Formål: træn GLM (Poisson/NegBin) med lag- og rullende features pr. team; gem test-forecast, metrics og model-objekter.
# - Træne forbedrede GLM-modeller pr. team
#   med både kalender- og lagfeatures
# - Distributioner: Poisson + Negativ Binomial
# - Train: 2022-01-01 → 2024-12-31
# - Test:  2025-01-01 → 2025-11-11 (eller sidste timestamp)
# - Input:  ts_hourly_all_teams_lags.rds  (trin 07)
# - Output:
#     models/glm_lags_by_team.rds
#     data_processed/fc_test_glm_lags_by_team.rds
#     data_processed/metrics_test_glm_lags_by_team.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(MASS)    # glm.nb
library(jsonlite)

# undgå konflikter med MASS::select osv.
select <- dplyr::select
filter <- dplyr::filter
lag    <- dplyr::lag

# ---- paths ----
in_path          <- here("data_processed", "ts_hourly_all_teams_lags.rds")
models_out_path  <- here("models", "glm_lags_by_team.rds")
fc_test_out_path <- here("data_processed", "fc_test_glm_lags_by_team.rds")
metrics_out_path <- here("data_processed", "metrics_test_glm_lags_by_team.rds")

if (!dir.exists(here("models"))) {
  dir.create(here("models"), recursive = TRUE)
}

# ---- load data ----
ts_lags <- readRDS(in_path)

stopifnot(all(c("team", "ds", "y") %in% names(ts_lags)))
stopifnot(inherits(ts_lags$ds, "POSIXct"))

# ---- train / test perioder ----
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
  test_end_dt <- max(ts_lags$ds)
}

train_end_date <- as.Date(test_start_dt) - days(1)

message("Train: <  ", train_end_date)
message("Test:  >= ", test_start_dt, "  &  <= ", test_end_dt)

# ---- featurevalg ----
calendar_features <- c(
  "hour",
  "weekday",      # faktor fra trin 06
  "month",
  "Vinterferie",
  "Påskeferie",
  "Sommerferie",
  "Efterårsferie",
  "Juleferie"
)

lag_features <- c(
  "lag_1",
  "lag_24",
  "lag_48",
  "lag_168",
  "roll_mean_24",
  "roll_mean_72",
  "roll_mean_168"
)

all_features <- c(calendar_features, lag_features)

missing_feats <- setdiff(all_features, names(ts_lags))
if (length(missing_feats) > 0) {
  stop("Følgende forventede features mangler i ts_lags: ",
       paste(missing_feats, collapse = ", "))
}

# ---- helper: metrics ----
compute_metrics <- function(df, y_col, yhat_col, model_name, team_name) {
  y     <- df[[y_col]]
  y_hat <- df[[yhat_col]]
  
  residual <- y - y_hat
  
  tibble(
    team      = team_name,
    model     = model_name,
    n         = sum(!is.na(y) & !is.na(y_hat)),
    RMSE      = sqrt(mean((y - y_hat)^2, na.rm = TRUE)),
    MAE       = mean(abs(y - y_hat), na.rm = TRUE),
    MAPE      = mean(ifelse(y == 0, NA_real_, abs((y - y_hat) / y)), na.rm = TRUE),
    SMAPE     = mean(
      2 * abs(y - y_hat) / (abs(y) + abs(y_hat)),
      na.rm = TRUE
    ),
    Bias_mean = mean(residual, na.rm = TRUE),
    Bias_sd   = sd(residual, na.rm = TRUE)
  )
}

# ---- loop pr. team ----
teams <- sort(unique(ts_lags$team))

models_by_team <- list()
all_fc_test    <- list()
all_metrics    <- list()

for (tm in teams) {
  message("------ Team: ", tm, " ------")
  
  df_team <- ts_lags %>%
    filter(team == tm) %>%
    arrange(ds)
  
  # split
  train_df <- df_team %>%
    filter(ds < train_end_date + days(1))   # inkl. hele 2024
  test_df  <- df_team %>%
    filter(ds >= test_start_dt,
           ds <= test_end_dt)
  
  # drop NAs i lag-features (fortrinsvis i starten af serien)
  train_df <- train_df %>%
    drop_na(all_of(all_features))
  
  test_df <- test_df %>%
    drop_na(all_of(all_features))
  
  if (nrow(train_df) == 0 || nrow(test_df) == 0) {
    warning("Team ", tm, " har ikke nok data til træning/test – springes over.")
    next
  }
  
  # sørg for at faktorer i test har samme levels som i train
  train_df <- train_df %>%
    mutate(
      weekday = factor(weekday),
      month   = factor(month)
    )
  
  test_df <- test_df %>%
    mutate(
      weekday = factor(weekday, levels = levels(train_df$weekday)),
      month   = factor(month,   levels = levels(train_df$month))
    )
  
  # formel
  fml <- as.formula(
    paste(
      "y ~",
      paste(
        c(
          "factor(hour)",
          "weekday",
          "month",
          "Vinterferie",
          "Påskeferie",
          "Sommerferie",
          "Efterårsferie",
          "Juleferie",
          # lag / rolling
          "lag_1",
          "lag_24",
          "lag_48",
          "lag_168",
          "roll_mean_24",
          "roll_mean_72",
          "roll_mean_168"
        ),
        collapse = " + "
      )
    )
  )
  
  # ---- Poisson med lags ----
  message("  · Træner Poisson GLM + lags")
  mod_pois_lags <- glm(
    formula = fml,
    family  = poisson(link = "log"),
    data    = train_df
  )
  
  # ---- NegBin med lags ----
  message("  · Træner NegBin GLM + lags")
  mod_nb_lags <- tryCatch(
    MASS::glm.nb(formula = fml, data = train_df),
    error = function(e) {
      warning("NegBin (lags) konvergerede ikke for team ", tm,
              " – bruger kun Poisson. Fejl: ", e$message)
      NULL
    }
  )
  
  # ---- predict på test ----
  test_pred <- test_df %>%
    mutate(
      y_pois_lags = predict(
        mod_pois_lags,
        newdata = .,
        type   = "response"
      ),
      y_nb_lags   = if (!is.null(mod_nb_lags)) {
        predict(mod_nb_lags, newdata = ., type = "response")
      } else {
        NA_real_
      }
    )
  
  # ---- metrics ----
  metrics_pois <- compute_metrics(
    df         = test_pred,
    y_col      = "y",
    yhat_col   = "y_pois_lags",
    model_name = "GLM_Poisson_lags",
    team_name  = tm
  )
  
  metrics_list <- list(metrics_pois)
  
  if (!is.null(mod_nb_lags)) {
    metrics_nb <- compute_metrics(
      df         = test_pred,
      y_col      = "y",
      yhat_col   = "y_nb_lags",
      model_name = "GLM_NegBin_lags",
      team_name  = tm
    )
    metrics_list[[length(metrics_list) + 1]] <- metrics_nb
  }
  
  models_by_team[[tm]] <- list(
    poisson_lags = mod_pois_lags,
    negbin_lags  = mod_nb_lags
  )
  
  all_fc_test[[tm]] <- test_pred %>%
    mutate(team = tm) %>%
    select(team, ds, y, y_pois_lags, y_nb_lags)
  
  all_metrics[[tm]] <- bind_rows(metrics_list)
}

# ---- bind & save ----
fc_test_all <- bind_rows(all_fc_test)
metrics_all <- bind_rows(all_metrics)

saveRDS(models_by_team, models_out_path)
saveRDS(fc_test_all,    fc_test_out_path)
saveRDS(metrics_all,    metrics_out_path)

message("✔ GLM + lag-modeller trænet for alle teams")
message("✔ Test-forecast gemt: ", fc_test_out_path)
message("✔ Metrics gemt:       ", metrics_out_path)
message("✔ Model-objekter gemt:", models_out_path)
