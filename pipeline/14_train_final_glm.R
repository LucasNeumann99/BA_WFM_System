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
ts_path   <- here("data_processed", "ts_hourly_all_teams_struct_adj.rds")
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
# Feature types
# ------------------------------------------------------------
factor_vars <- c("hour", "weekday", "month")

numeric_vars <- c(
  "week", "year",
  "Juleferie", "Vinterferie", "Påskeferie",
  "Sommerferie", "Efterårsferie"
)

# ------------------------------------------------------------
# Train per team
# ------------------------------------------------------------
for (tm in teams) {
  
  message("---- Training NegBin GLM for team: ", tm)
  
  tr <- df_train %>% filter(team == tm)
  te <- df_test  %>% filter(team == tm)
  
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
  form <- y ~
    hour + weekday + month +
    week_c + year_c +
    Juleferie + Vinterferie + Påskeferie +
    Sommerferie + Efterårsferie
  
  # ---------------- Train model ----------------
  mod_nb <- glm.nb(formula = form, data = tr)
  
  # ---------------- Forecast ----------------
  te <- te %>%
    mutate(y_hat = predict(mod_nb, newdata = te, type = "response"))
  
  fc_list[[tm]]    <- te
  models_out[[tm]] <- mod_nb
}

# Save
saveRDS(models_out, out_model)
saveRDS(bind_rows(fc_list), out_fc)

message("✔ FINAL NegBin GLM baseline trained and saved.")
