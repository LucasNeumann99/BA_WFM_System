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

# ---- parametre til strukturel filtrering ----
team_no         <- "Team NO 1, Travelcare"
storebrand_code <- "C370"

# ---- fjern Storebrand fra Team NO på call-niveau ----
calls_filtered <- calls %>%
  filter(
    !(call_responsible_department_l5 == team_no &
        common_customer_code == storebrand_code)
  )

message("Total calls before filter: ", nrow(calls))
message("Total calls after  filter: ", nrow(calls_filtered))
message("Removed (Team NO, customer_code C370): ",
        nrow(calls) - nrow(calls_filtered))

# ---- aggregate hourly per team ----
ts_hourly <- calls_filtered %>%
  mutate(
    ds   = floor_date(call_start_time, unit = "hour"),
    team = call_responsible_department_l5
  ) %>%
  group_by(team, ds) %>%
  summarise(y = sum(real_offered_call, na.rm = TRUE), .groups = "drop") %>%
  arrange(team, ds)

# ---- save ----
saveRDS(ts_hourly, out_path)

message("✔ Hourly time series created for all teams (ds, y, team)")
message("✔ Team NO is now WITHOUT customer_code 370 in y")
