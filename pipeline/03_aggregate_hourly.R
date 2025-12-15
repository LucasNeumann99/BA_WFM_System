# pipeline/03_aggregate_hourly.R
library(tidyverse)
library(lubridate)
library(here)

# ---- paths ----
in_path  <- here("data_processed", "calls_clean.rds")
out_path <- here("data_processed", "ts_hourly_all_teams.rds")

# ---- load ----
calls <- readRDS(in_path)

# ---- sanity check ----
stopifnot(all(c(
  "call_start_time",
  "call_responsible_department_l5"
) %in% names(calls)))

# ---- aggregate hourly per team ----
ts_hourly <- calls %>%
  mutate(
    ds   = floor_date(call_start_time, unit = "hour"),
    team = call_responsible_department_l5
  ) %>%
  count(team, ds, name = "y") %>%
  arrange(team, ds)

# ---- save ----
saveRDS(ts_hourly, out_path)

message("✔ Hourly time series created for all teams (ds, y, team)")
