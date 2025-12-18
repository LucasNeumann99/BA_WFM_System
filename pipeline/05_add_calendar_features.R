# pipeline/05_add_calendar_features.R
# Formål: tilføj kalenderfeatures (helligdage, ferier) via helpers i model_functions/.
library(tidyverse)
library(here)

source(here("model_functions", "calendar_features.R"))

# ---- paths ----
in_path  <- here("data_processed", "ts_hourly_all_hours.rds")
out_path <- here("data_processed", "ts_hourly_all_hours_calendar.rds")

# ---- load ----
ts <- readRDS(in_path)

# ---- team → country mapping ----
team_country_map <- tibble(
  team = c(
    "Team SE 1, Travelcare",
    "Team SE 2, Travelcare",
    "Team DK 1, Travelcare",
    "Team NO 1, Travelcare",
    "Team FI 1, Travelcare"
  ),
  country = c("SE", "SE", "DK", "NO", "FI")
)

ts <- ts %>%
  left_join(team_country_map, by = "team")

stopifnot(!any(is.na(ts$country)))

# ---- add holidays per country ----
ts_calendar <- ts %>%
  group_by(team, country) %>%
  group_modify(~ add_country_holidays(.x, country_code = .y$country[1])) %>%
  ungroup()

# ---- save ----
saveRDS(ts_calendar, out_path)

message("✔ Calendar features added for all teams")
