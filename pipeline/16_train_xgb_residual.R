# ============================================================
# 16_train_xgb_residual.R
# ------------------------------------------------------------
# Formaal: traen XGBoost-residualmodeller pr. team oven paa final GLM.
# Output: models/final_xgb_residual_by_team.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(xgboost)

# ------------------------------------------------------------
# Load data and GLM models
# ------------------------------------------------------------
models <- readRDS(here("models", "final_glm_negbin_by_team.rds"))
df_base <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))

train_cutoff <- ymd("2024-12-31")
year_ref     <- 2024

xgb_params <- list(
  objective        = "reg:squarederror",
  eta              = 0.05,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8
)

xgb_resid_by_team <- list()

for (tm in names(models)) {
  message("---- Training XGB residuals for team: ", tm)
  mod <- models[[tm]]
  if (is.null(mod)) next

  tr <- df_base %>% filter(team == tm, ds < train_cutoff)
  if (nrow(tr) == 0) next

  tr <- tr %>%
    mutate(
      hour    = factor(hour),
      weekday = factor(weekday, ordered = FALSE),
      month   = factor(month),
      year_c  = as.numeric(year) - year_ref
    ) %>%
    droplevels()

  num_vars_other <- c(
    "Juleferie", "Vinterferie", "Påskeferie",
    "Sommerferie", "Efterårsferie"
  )

  tr <- tr %>% mutate(across(all_of(num_vars_other), as.numeric))

  y_hat_glm_train <- predict(mod, newdata = tr, type = "response")
  resid <- tr$y - y_hat_glm_train

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
    )

  X_train <- model.matrix(
    ~ hour + weekday + month + week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie - 1,
    data = fe_train
  )

  dtrain <- xgb.DMatrix(X_train, label = resid)
  xgb_fit <- xgb.train(
    params  = xgb_params,
    data    = dtrain,
    nrounds = 300,
    verbose = 0
  )

  xgb_resid_by_team[[tm]] <- list(
    model_raw = xgb.serialize(xgb_fit),
    feature_names = colnames(X_train)
  )
}

saveRDS(xgb_resid_by_team, here("models", "final_xgb_residual_by_team.rds"))
message("✔ XGB residual models saved: ", here("models", "final_xgb_residual_by_team.rds"))
