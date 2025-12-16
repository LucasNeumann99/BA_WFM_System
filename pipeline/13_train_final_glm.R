# ============================================================
# 13_train_final_glm.R (CLEAN + CORRECT)
# FINAL Negative Binomial GLM baseline
# - Train 24h
# - Save models + forecasts
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(MASS)

message("Training FINAL NegBin GLM baseline (24h).")

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
ts_path   <- here("data_processed", "ts_hourly_all_teams_features.rds")
out_model <- here("models", "final_glm_negbin_by_team.rds")
out_fc    <- here("results", "final", "glm", "fc_final_glm_negbin.rds")

dir.create(here("results", "final", "glm"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
df <- readRDS(ts_path)

train_cutoff <- ymd("2024-12-31")

df_train <- df %>% filter(ds < train_cutoff)
df_test  <- df %>% filter(ds >= ymd("2025-01-01"))

teams <- unique(df$team)

# ------------------------------------------------------------
# Feature types (NO Holiday_any to avoid multicollinearity)
# ------------------------------------------------------------
factor_vars <- c("hour", "weekday", "month")

numeric_vars <- c(
  "week", "year",
  "is_weekend", "is_night",
  "Juleferie", "Vinterferie", "Påskeferie",
  "Sommerferie", "Efterårsferie"
)

# Output containers
models_out <- list()
fc_list <- list()

# ------------------------------------------------------------
# Train per team
# ------------------------------------------------------------
for (tm in teams) {
  
  message("---- Training NegBin GLM for team: ", tm)
  
  tr <- df_train %>% filter(team == tm)
  te <- df_test  %>% filter(team == tm)
  
  # Clean factor levels
  tr <- tr %>% mutate(across(all_of(factor_vars), factor)) %>% droplevels()
  
  te <- te %>% mutate(across(all_of(factor_vars),
                             ~ factor(.x, levels = levels(tr[[cur_column()]]))))
  
  tr <- tr %>% mutate(across(all_of(numeric_vars), as.numeric))
  te <- te %>% mutate(across(all_of(numeric_vars), as.numeric))
  
  # ---------------- Model formula ----------------
  form <- as.formula(
    paste(
      "y ~",
      paste(
        c(
          "hour", "weekday", "month",
          "week", "year",
          "is_weekend", "is_night",
          "Juleferie", "Vinterferie", "Påskeferie",
          "Sommerferie", "Efterårsferie"
        ),
        collapse = " + "
      )
    )
  )
  
  # ---------------- Train model ----------------
  mod_nb <- glm.nb(formula = form, data = tr)
  
  # ---------------- Forecast ----------------
  te <- te %>% mutate(y_hat = predict(mod_nb, newdata = te, type = "response"))
  
  fc_list[[tm]]     <- te
  models_out[[tm]]  <- mod_nb
}

# Save
saveRDS(models_out, out_model)
saveRDS(bind_rows(fc_list), out_fc)

message("✔ FINAL NegBin GLM baseline trained and saved.")
