#!/usr/bin/env Rscript
# ============================================================
# 29_spx_vs_shift_plan_single_day.R
# ------------------------------------------------------------
# Single-day SPX vs planned shifts comparison (DK/FI/NO).
# Output: BA_WFM_Output/Manning/extra_analytics
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "Manning", "extra_analytics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

target_day <- as.Date("2026-01-01")

# Load SPX
spx_path <- here("data_raw", "Prognosedata_til_SPX_pr_240925.csv")
spx_raw <- readr::read_delim(spx_path, delim = ";", skip = 1, col_types = readr::cols(.default = "c"))
spx_raw <- spx_raw %>%
  select(where(~ !all(is.na(.x)))) %>%
  mutate(across(everything(), ~ str_trim(as.character(.x))))

spx <- spx_raw %>%
  mutate(
    Dato = mdy(Dato),
    across(-c(Uge, Ugedag, Dato), ~ na_if(.x, "-")),
    across(-c(Uge, Ugedag, Dato), ~ as.numeric(str_replace(.x, ",", ".")))
  )

spx_map <- c(
  "Team Carring" = "Team DK 1, Travelcare",
  "Team Finsk" = "Team FI 1, Travelcare",
  "Team IF" = "Team NO 1, Travelcare",
  "Team Inside" = "Team SE 1, Travelcare",
  "Team Tryg" = "Team SE 2, Travelcare"
)

spx_long <- spx %>%
  select(Dato, all_of(names(spx_map))) %>%
  pivot_longer(cols = -Dato, names_to = "spx_team", values_to = "spx_value") %>%
  mutate(team = recode(spx_team, !!!spx_map)) %>%
  filter(!is.na(spx_value))

spx_day <- spx_long %>%
  filter(Dato == target_day)

# Load shift plans
staff_base <- file.path(paths$output, "Manning")
teams <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare"
)

shift_files <- file.path(staff_base, teams, "staffing", "shift_plan_optimized_v2.csv")
shift_files <- shift_files[file.exists(shift_files)]

shift_raw <- map_dfr(shift_files, ~ readr::read_csv(.x, show_col_types = FALSE))
shift_daily <- shift_raw %>%
  mutate(staff_date = as.Date(staff_date)) %>%
  group_by(team, staff_date) %>%
  summarise(planned_agents = sum(agents, na.rm = TRUE), .groups = "drop")

shift_day <- shift_daily %>%
  filter(staff_date == target_day)

cmp <- spx_day %>%
  left_join(shift_day, by = "team") %>%
  transmute(
    date = target_day,
    team,
    spx_team,
    spx_value,
    planned_agents
  )

readr::write_csv(
  cmp,
  file.path(out_dir, "spx_vs_shiftplan_2026-01-01.csv")
)

message("Saved: ", file.path(out_dir, "spx_vs_shiftplan_2026-01-01.csv"))
