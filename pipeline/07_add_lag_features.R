# ============================================================
# 07_add_lag_features.R
# ------------------------------------------------------------
# Formål:
# - Tilføje lag- og rullende features pr. team
# - Kun baseret på historisk y
# - Accepterer NA i starten (trimmes senere)
# - Klar til forecast-modeller (ML / stats / Erlang input)
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(zoo)

# ---- paths ----
in_path  <- here("data_processed", "ts_hourly_all_teams_features.rds")
out_path <- here("data_processed", "ts_hourly_all_teams_lags.rds")

# ---- load ----
ts <- readRDS(in_path)

# ---- sanity checks ----
stopifnot(all(c("team", "ds", "y") %in% names(ts)))
stopifnot(inherits(ts$ds, "POSIXct"))

# ---- add lag features per team ----
ts_lags <- ts %>%
  group_by(team) %>%
  arrange(ds) %>%
  mutate(
    # -------------------------------
    # Classic lags
    # -------------------------------
    lag_1   = lag(y, 1),     # sidste time
    lag_24  = lag(y, 24),    # samme time i går
    lag_48  = lag(y, 48),
    lag_168 = lag(y, 168),   # samme time sidste uge
    
    # -------------------------------
    # Rolling means (smooth demand)
    # -------------------------------
    roll_mean_24  = rollmean(y, 24,  fill = NA, align = "right"),
    roll_mean_72  = rollmean(y, 72,  fill = NA, align = "right"),
    roll_mean_168 = rollmean(y, 168, fill = NA, align = "right"),
    
    # -------------------------------
    # Rolling sums (volumen)
    # -------------------------------
    roll_sum_24  = rollsum(y, 24,  fill = NA, align = "right"),
    roll_sum_168 = rollsum(y, 168, fill = NA, align = "right")
  ) %>%
  ungroup()

# ---- sanity checks ----
# y må aldrig være NA
stopifnot(!any(is.na(ts_lags$y)))

# lags MÅ gerne have NA i starten
message("Lag NA counts (expected):")
print(
  ts_lags %>%
    summarise(across(starts_with("lag_"), ~ sum(is.na(.x))))
)

# ---- save ----
saveRDS(ts_lags, out_path)

message("✔ Lag features added")
message("✔ NA values only in lag/rolling features (expected)")
message("✔ Dataset ready for model trimming & training")
