# ============================================================
# 07_structural_adjustments.R
# ------------------------------------------------------------
# Formål:
# - Læg strukturelle, forretningsdrevne ændringer ind i y
#   (fx:
#      * Team NO: fjerne kunde (Storebrand, rapporteret som customer_code 370)
#      * Team SE: evt. split mellem SE1/SE2 (TODO)
#      * Team DK: fremtidig vol. justering (kun efter testperiode)
#
# Input:
#   - data_processed/ts_hourly_all_teams_features.rds  (step 06)
#   - data_processed/calls_clean.rds                   (call-niveau)
#
# Output:
#   - data_processed/ts_hourly_all_teams_struct_adj.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

# ---- paths ----
ts_path    <- here("data_processed", "ts_hourly_all_teams_features.rds")
calls_path <- here("data_processed", "calls_clean.rds")
out_path   <- here("data_processed", "ts_hourly_all_teams_struct_adj.rds")

# ---- load data ----
ts    <- readRDS(ts_path)
calls <- readRDS(calls_path)

# sanity
stopifnot(all(c("team", "ds", "y") %in% names(ts)))
stopifnot(inherits(ts$ds, "POSIXct"))

ts <- ts %>% mutate(date = as.Date(ds))

message("Rows before structural adjustments: ", nrow(ts))
message("Teams: ", paste(unique(ts$team), collapse = " | "))

# ------------------------------------------------------------
# 1) Team NO – fjern Storebrand-volumen (customer_code 370 i rapporten)
#    -> baseret på service_entrance, da customer_code ikke findes i calls_clean
# ------------------------------------------------------------

adjust_team_no_storebrand <- function(ts, calls) {
  team_no <- "Team NO 1, Travelcare"
  
  # Sikkerhed: tjek at de kolonner vi skal bruge findes
  needed_cols <- c("call_start_time", "call_responsible_department_l5", "service_entrance")
  missing_cols <- setdiff(needed_cols, names(calls))
  if (length(missing_cols) > 0) {
    warning(
      "Kan ikke lave Team NO/Storebrand-justering – mangler kolonner i calls_clean: ",
      paste(missing_cols, collapse = ", ")
    )
    return(ts)
  }
  
  # 1) Find de service_entrances for Team NO, der tilhører den kunde vi vil fjerne.
  #    Her bruger vi navnet i data til at identificere dem, men i rapporten kan vi
  #    omtale den som 'customer_code 370'.
  storebrand_ents <- calls %>%
    filter(
      call_responsible_department_l5 == team_no,
      !is.na(service_entrance),
      str_detect(service_entrance, "Storebrand")
    ) %>%
    distinct(service_entrance) %>%
    pull(service_entrance)
  
  if (length(storebrand_ents) == 0) {
    warning("Fandt ingen service_entrances med 'Storebrand' for Team NO – ingen justering foretaget.")
    return(ts)
  }
  
  message("Team NO / Storebrand – identificerede service_entrances (rapporteres som customer_code 370):")
  message("  ", paste(storebrand_ents, collapse = " | "))
  
  # 2) Byg hourly time series for disse service_entrances på Team NO
  storebrand_ts <- calls %>%
    filter(
      call_responsible_department_l5 == team_no,
      service_entrance %in% storebrand_ents
    ) %>%
    mutate(
      ds   = floor_date(call_start_time, "hour"),
      team = call_responsible_department_l5
    ) %>%
    count(team, ds, name = "y_storebrand")
  
  message("Team NO / Storebrand – antal time-rækker: ", nrow(storebrand_ts))
  
  # 3) Join ind i ts og træk volumen fra
  ts %>%
    left_join(storebrand_ts, by = c("team", "ds")) %>%
    mutate(
      y_storebrand = replace_na(y_storebrand, 0L),
      y_adj        = pmax(y - y_storebrand, 0L),
      y            = if_else(team == team_no, y_adj, y)
    ) %>%
    select(-y_storebrand, -y_adj)
}

# ------------------------------------------------------------
# 2) Team SE – split mellem SE1 og SE2 (skelet / TODO)
# ------------------------------------------------------------

adjust_team_se_split <- function(ts) {
  # TODO: udfyld konkret logik når du har fastlagt præcis dato + fordelingsnøgler.
  # Lige nu returnerer vi ts uændret.
  ts
}

# ------------------------------------------------------------
# 3) Team DK – fremtidig procent-ændring (efter testperiode)
# ------------------------------------------------------------

adjust_team_dk_future <- function(ts) {
  team_dk       <- "Team DK 1, Travelcare"
  change_date   <- as.Date("2026-05-01")  # VIGTIGT: efter test-slut (2025-11-11)
  factor_change <- 0.6                    # fx -40% forventet fremover
  
  ts %>%
    mutate(
      y = case_when(
        team == team_dk & date >= change_date ~ y * factor_change,
        TRUE                                  ~ y
      )
    )
}

# ------------------------------------------------------------
# 4) Kør alle justeringer i en pipeline
# ------------------------------------------------------------

ts_adj <- ts %>%
  adjust_team_no_storebrand(calls) %>%
  adjust_team_se_split() %>%
  adjust_team_dk_future()

message("Rows after structural adjustments (should be same): ", nrow(ts_adj))

# ------------------------------------------------------------
# 5) Gem resultat til næste step
# ------------------------------------------------------------

saveRDS(ts_adj, out_path)

message("✔ Structural adjustments saved to: ", out_path)
