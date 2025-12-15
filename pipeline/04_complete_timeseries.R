# ============================================================
# 04_complete_timeseries.R 
# ------------------------------------------------------------
# Formål:
# - Sikre FULD tidsakse (hourly) for ALLE teams
# - Udfylde manglende timer med y = 0
# - Fundament for kalender- og tidsfeatures
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

# ---- paths ----
in_path  <- here("data_processed", "ts_hourly_all_teams.rds")
out_path <- here("data_processed", "ts_hourly_all_hours.rds")

# ---- load ----
ts_hourly <- readRDS(in_path)

# ---- sanity checks ----
stopifnot(all(c("team", "ds", "y") %in% names(ts_hourly)))
stopifnot(inherits(ts_hourly$ds, "POSIXct"))

# ---- find global time bounds ----
min_ds_raw <- min(ts_hourly$ds)
max_ds     <- max(ts_hourly$ds)

min_ds <- floor_date(min_ds_raw, unit = "hour")
max_ds <- floor_date(max_ds, unit = "hour")


message("Time range:")
message("  From: ", min_ds)
message("  To:   ", max_ds)

# ---- full hourly timeline ----
full_time <- tibble(
  ds = seq(
    from = min_ds,
    to   = max_ds,
    by   = "1 hour"
  )
)

# ---- all teams ----
teams <- ts_hourly %>%
  distinct(team) %>%
  arrange(team)

# ---- cartesian grid: team x time ----
full_grid <- tidyr::crossing(
  teams,
  full_time
)

# ---- join observed data ----
ts_complete <- full_grid %>%
  left_join(
    ts_hourly,
    by = c("team", "ds")
  ) %>%
  mutate(
    y = replace_na(y, 0L)
  ) %>%
  arrange(team, ds)

# ---- final sanity checks ----
# 1) No missing y
stopifnot(!any(is.na(ts_complete$y)))

# 2) Identical time axis per team
check_time_axis <- ts_complete %>%
  group_by(team) %>%
  summarise(
    n_hours = n(),
    min_ds  = min(ds),
    max_ds  = max(ds),
    .groups = "drop"
  )

stopifnot(length(unique(check_time_axis$n_hours)) == 1)

# ---- save ----
saveRDS(ts_complete, out_path)

message("✔ Complete hourly time series created")
message("✔ All teams share identical time axis")
message("✔ Missing hours filled with y = 0")
