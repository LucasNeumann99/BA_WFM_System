# ============================================================
# 06_add_time_features.R
# ------------------------------------------------------------
# Formål:
# - Tilføje faste tidsfeatures (kalender)
# - Ingen brug af historiske værdier
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

# ---- paths ----
in_path  <- here("data_processed", "ts_hourly_all_hours_calendar.rds")
out_path <- here("data_processed", "ts_hourly_all_teams_features.rds")

# ---- load ----
ts <- readRDS(in_path)

# ---- sanity checks ----
stopifnot(all(c("ds", "team") %in% names(ts)))
stopifnot(inherits(ts$ds, "POSIXct"))

# ---- add time features ----
ts_time <- ts %>%
  mutate(
    hour       = hour(ds),
    weekday    = wday(ds, label = TRUE, week_start = 1),
    week       = isoweek(ds),
    month      = month(ds),
    year       = year(ds),
    is_weekend = as.integer(wday(ds, week_start = 1) >= 6),
    is_night   = as.integer(hour < 7 | hour >= 22)
  )

# ---- final checks ----
stopifnot(!any(is.na(ts_time$hour)))
stopifnot(!any(is.na(ts_time$weekday)))
stopifnot(!any(is.na(ts_time$week)))

# ---- save ----
saveRDS(ts_time, out_path)

message("✔ Time features added (calendar only)")
