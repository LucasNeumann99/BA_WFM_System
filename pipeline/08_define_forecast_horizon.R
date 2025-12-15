# ============================================================
# 08_define_forecast_horizon.R
# ------------------------------------------------------------
# Formål:
# - Definere fremtidig forecast-horisont (hourly)
# - Identisk feature-struktur som historiske data
# - Kun kalender- og tidsfeatures (ingen lags)
# - y = NA (ingen datalæk)
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "calendar_features.R"))

# ---- paths ----
in_path  <- here("data_processed", "ts_hourly_all_teams_features.rds")
out_path <- here("data_processed", "ts_future_all_teams_features.rds")

# ---- load historical data (for team list only) ----
hist_ts <- readRDS(in_path)

teams <- hist_ts %>%
  distinct(team) %>%
  arrange(team)

# ---- forecast horizon (juster efter behov) ----
forecast_start <- ymd_hms("2025-01-01 00:00:00", tz = "UTC")
forecast_end   <- ymd_hms("2028-12-31 23:00:00", tz = "UTC")

stopifnot(forecast_start < forecast_end)

message("Forecast horizon:")
message("  From: ", forecast_start)
message("  To:   ", forecast_end)

# ---- full hourly future timeline ----
future_time <- tibble(
  ds = seq(
    from = forecast_start,
    to   = forecast_end,
    by   = "1 hour"
  )
)

# ---- cartesian grid: team x future time ----
future_grid <- tidyr::crossing(
  teams,
  future_time
)

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

future_grid <- future_grid %>%
  left_join(team_country_map, by = "team")

stopifnot(!any(is.na(future_grid$country)))

# ---- add calendar features ----
ts_future <- future_grid %>%
  group_by(team, country) %>%
  group_modify(~ add_country_holidays(.x, country_code = .y$country[1])) %>%
  ungroup()

# ---- add time features (samme som trin 06) ----
ts_future <- ts_future %>%
  mutate(
    hour       = hour(ds),
    weekday    = wday(ds, label = TRUE, week_start = 1),
    week       = isoweek(ds),
    month      = month(ds),
    year       = year(ds),
    is_weekend = as.integer(wday(ds, week_start = 1) >= 6),
    is_night   = as.integer(hour < 7 | hour >= 22)
  )

# ---- y = NA (vigtigt for ingen datalæk) ----
ts_future <- ts_future %>%
  mutate(y = NA_integer_)

# ---- sanity ----
stopifnot(all(is.na(ts_future$y)))

check_axis <- ts_future %>%
  group_by(team) %>%
  summarise(
    n_hours = n(),
    min_ds  = min(ds),
    max_ds  = max(ds),
    .groups = "drop"
  )

stopifnot(length(unique(check_axis$n_hours)) == 1)

# ---- save ----
saveRDS(ts_future, out_path)

message("✔ Future forecast horizon created")
message("✔ Calendar + time features added")
message("✔ Identical hourly time axis for all teams")
message("✔ y correctly set to NA")
