# ============================================================
# 14_train_final_glm.R (CLEAN + CORRECT)
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
ts_baseline_path <- here("data_processed", "ts_hourly_all_teams_struct_adj.rds")
ts_lags_path     <- here("data_processed", "ts_hourly_all_teams_lags.rds")
out_model        <- here("models", "final_glm_negbin_by_team.rds")
out_fc           <- here("results", "final", "glm", "fc_final_glm_negbin.rds")

dir.create(here("results", "final", "glm"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
df_base <- readRDS(ts_baseline_path)
df_lags <- readRDS(ts_lags_path)

train_cutoff <- ymd("2024-12-31")

df_train_base <- df_base %>% filter(ds < train_cutoff)
df_test_base  <- df_base %>% filter(ds >= ymd("2025-01-01"))

# Teams der skal bruge lag-features (fra uplift-analysen)
teams_use_lags <- c(
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare"
)

teams <- unique(df_base$team)
models_out <- list()
fc_list    <- list()

# ------------------------------------------------------------
# Feature types
# ------------------------------------------------------------
factor_vars <- c("hour", "weekday", "month")

numeric_vars <- c(
  "week", "year",
  "Juleferie", "Vinterferie", "Påskeferie",
  "Sommerferie", "Efterårsferie"
)

lag_vars <- c(
  "lag_1", "lag_24", "lag_48", "lag_168",
  "roll_mean_24", "roll_mean_72", "roll_mean_168"
)

# ------------------------------------------------------------
# Train per team
# ------------------------------------------------------------
for (tm in teams) {
  
  message("---- Training NegBin GLM for team: ", tm)
  
  use_lags <- tm %in% teams_use_lags
  
  if (use_lags) {
    tr <- df_lags %>%
      filter(team == tm, ds < train_cutoff) %>%
      drop_na(all_of(lag_vars))  # fjern tidlige rækker uden lag
    
    te <- df_lags %>%
      filter(team == tm, ds >= ymd("2025-01-01")) %>%
      drop_na(all_of(lag_vars))  # behold kun hvor lag findes
  } else {
    tr <- df_train_base %>% filter(team == tm)
    te <- df_test_base  %>% filter(team == tm)
  }
  
  if (nrow(tr) == 0 || nrow(te) == 0) {
    warning("Ingen data til træning/forecast for ", tm, " (use_lags = ", use_lags, "). Springes over.")
    next
  }
  
  # 1) Clean factor levels + sæt baseline for faktorer
  tr <- tr %>%
    mutate(
      hour    = factor(hour),             # baseline = første level (typisk 0)
      weekday = factor(weekday, ordered = FALSE),
      month   = factor(month)
    ) %>%
    droplevels()
  
  te <- te %>%
    mutate(
      hour    = factor(hour,    levels = levels(tr$hour)),
      weekday = factor(weekday, levels = levels(tr$weekday)),
      month   = factor(month,   levels = levels(tr$month))
    )
  
  # 2) Center week/year for pænere intercept
  week_mean <- mean(tr$week, na.rm = TRUE)
  year_ref  <- 2024  # vi vil gerne tolke intercept som "omkring år 2024"
  
  tr <- tr %>%
    mutate(
      week_c = as.numeric(week) - week_mean,
      year_c = as.numeric(year) - year_ref
    )
  
  te <- te %>%
    mutate(
      week_c = as.numeric(week) - week_mean,
      year_c = as.numeric(year) - year_ref
    )
  
  # øvrige numeriske features som numeric
  num_vars_other <- c("Juleferie", "Vinterferie", "Påskeferie",
                      "Sommerferie", "Efterårsferie")
  
  tr <- tr %>%
    mutate(across(all_of(num_vars_other), as.numeric))
  te <- te %>%
    mutate(across(all_of(num_vars_other), as.numeric))
  
  # ---------------- Model formula ----------------
  if (use_lags) {
    form <- y ~
      hour + weekday + month +
      week_c + year_c +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie +
      lag_1 + lag_24 + lag_48 + lag_168 +
      roll_mean_24 + roll_mean_72 + roll_mean_168
  } else {
    form <- y ~
      hour + weekday + month +
      week_c + year_c +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie
  }
  
  # ---------------- Train model ----------------
  mod_nb <- glm.nb(formula = form, data = tr)
  
  # ---------------- Forecast ----------------
  te <- te %>%
    mutate(
      y_hat  = predict(mod_nb, newdata = te, type = "response"),
      model_used = if (use_lags) "GLM_NegBin_lags" else "GLM_NegBin_baseline"
    )
  
  fc_list[[tm]]    <- te
  models_out[[tm]] <- mod_nb
}

# Save
saveRDS(models_out, out_model)
saveRDS(bind_rows(fc_list), out_fc)

message("✔ FINAL NegBin GLM baseline trained and saved.")
