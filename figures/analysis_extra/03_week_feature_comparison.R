# ============================================================
# 03_week_feature_comparison.R
# ------------------------------------------------------------
# Formål:
# - Sammenligne NegBin GLM med og uden week-feature
# - Ens train/test som final GLM:
#   - Train: < 2024-12-31
#   - Test:  >= 2025-01-01 til sidste timestamp
#
# Output:
# - CSV: figures/analysis_extra/metrics_week_feature_comparison.csv
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(MASS)

in_path <- here("data_processed", "ts_hourly_all_teams_struct_adj.rds")
out_path <- here("figures", "analysis_extra", "metrics_week_feature_comparison.csv")

df <- readRDS(in_path)

train_end_date <- ymd("2024-12-31")
test_start_dt  <- ymd_hms("2025-01-01 00:00:00", tz = "UTC")
test_end_dt    <- max(df$ds)
year_ref <- 2024

compute_metrics <- function(df) {
  df %>%
    summarise(
      n = sum(!is.na(y_hat) & !is.na(y)),
      RMSE = sqrt(mean((y - y_hat)^2, na.rm = TRUE)),
      MAE  = mean(abs(y - y_hat), na.rm = TRUE),
      MAPE = mean(abs(y - y_hat) / pmax(y, 1), na.rm = TRUE),
      SMAPE = mean(abs(y - y_hat) / ((abs(y) + abs(y_hat)) / 2), na.rm = TRUE),
      Bias_mean = mean(y_hat - y, na.rm = TRUE),
      Bias_sd   = sd(y_hat - y, na.rm = TRUE)
    )
}

fit_and_eval <- function(df_team, include_week) {
  tr <- df_team %>%
    filter(ds < train_end_date + days(1)) %>%
    mutate(
      hour    = factor(hour),
      weekday = factor(weekday, ordered = FALSE),
      month   = factor(month),
      year_c  = as.numeric(year) - year_ref
    ) %>%
    droplevels()
  
  te <- df_team %>%
    filter(ds >= test_start_dt, ds <= test_end_dt) %>%
    mutate(
      hour    = factor(hour,    levels = levels(tr$hour)),
      weekday = factor(weekday, levels = levels(tr$weekday)),
      month   = factor(month,   levels = levels(tr$month)),
      year_c  = as.numeric(year) - year_ref
    )
  
  if (nrow(tr) == 0 || nrow(te) == 0) {
    return(NULL)
  }
  
  form <- if (include_week) {
    y ~ hour + weekday + month + factor(week) + year_c +
      Vinterferie + Påskeferie + Sommerferie + Efterårsferie + Juleferie
  } else {
    y ~ hour + weekday + month + year_c +
      Vinterferie + Påskeferie + Sommerferie + Efterårsferie + Juleferie
  }
  
  mod <- tryCatch(
    MASS::glm.nb(formula = form, data = tr),
    error = function(e) NULL
  )
  
  if (is.null(mod)) {
    return(NULL)
  }
  
  te <- te %>%
    mutate(y_hat = predict(mod, newdata = te, type = "response"))
  
  compute_metrics(te)
}

teams <- sort(unique(df$team))
metrics_all <- list()

for (tm in teams) {
  df_team <- df %>% filter(team == tm)
  
  m_no_week <- fit_and_eval(df_team, include_week = FALSE)
  if (!is.null(m_no_week)) {
    metrics_all[[length(metrics_all) + 1]] <- m_no_week %>%
      mutate(team = tm, model = "NegBin_no_week")
  }
  
  m_week <- fit_and_eval(df_team, include_week = TRUE)
  if (!is.null(m_week)) {
    metrics_all[[length(metrics_all) + 1]] <- m_week %>%
      mutate(team = tm, model = "NegBin_with_week")
  }
}

metrics_out <- bind_rows(metrics_all) %>%
  dplyr::select(team, model, everything())

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(metrics_out, out_path)

message("✔ Week comparison metrics saved: ", out_path)
