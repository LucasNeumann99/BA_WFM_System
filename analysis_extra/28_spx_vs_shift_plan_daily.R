#!/usr/bin/env Rscript
# ============================================================
# 28_spx_vs_shift_plan_daily.R
# ------------------------------------------------------------
# Compare SPX daily values vs planned shift counts per day
# (sum of agents across shifts) for mapped teams.
# Output: BA_WFM_Output/analysis_extra/Y_præsentation
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(scales)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "Manning", "extra_analytics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Load SPX data (semicolon, first row is "Tabel 1")
# ------------------------------------------------------------
spx_path <- here("data_raw", "Prognosedata_til_SPX_pr_240925.csv")
spx_raw <- readr::read_delim(spx_path, delim = ";", skip = 1, col_types = readr::cols(.default = "c"))
spx_raw <- spx_raw %>%
  dplyr::select(dplyr::where(~ !all(is.na(.x)))) %>%
  mutate(across(everything(), ~ str_trim(as.character(.x))))

spx <- spx_raw %>%
  mutate(
    Dato = mdy(Dato),
    across(-c(Uge, Ugedag, Dato), ~ na_if(.x, "-")),
    across(-c(Uge, Ugedag, Dato), ~ as.numeric(str_replace(.x, ",", ".")))
  )

# Map SPX columns to our teams
spx_map <- c(
  "Team Carring" = "Team DK 1, Travelcare",
  "Team Finsk" = "Team FI 1, Travelcare",
  "Team IF" = "Team NO 1, Travelcare",
  "Team Inside" = "Team SE 1, Travelcare",
  "Team Tryg" = "Team SE 2, Travelcare"
)

spx_long <- spx %>%
  dplyr::select(Dato, all_of(names(spx_map))) %>%
  pivot_longer(cols = -Dato, names_to = "spx_team", values_to = "spx_value") %>%
  mutate(team = recode(spx_team, !!!spx_map)) %>%
  filter(!is.na(spx_value))

# ------------------------------------------------------------
# Planned shifts per day from shift_plan_optimized_v2.csv
# ------------------------------------------------------------
staff_base <- file.path(paths$output, "Manning")
teams <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare",
  "Team SE 2, Travelcare"
)

shift_files <- file.path(staff_base, teams, "staffing", "shift_plan_optimized_v2.csv")
shift_files <- shift_files[file.exists(shift_files)]

shift_raw <- map_dfr(shift_files, ~ readr::read_csv(.x, show_col_types = FALSE))

shift_daily <- shift_raw %>%
  mutate(staff_date = as.Date(staff_date)) %>%
  group_by(team, staff_date) %>%
  summarise(planned_agents = sum(agents, na.rm = TRUE), .groups = "drop")

# SE total = SE1 + SE2
se_total <- shift_daily %>%
  filter(team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare")) %>%
  group_by(staff_date) %>%
  summarise(planned_agents = sum(planned_agents, na.rm = TRUE), .groups = "drop") %>%
  mutate(team = "Team SE total")

shift_daily <- shift_daily %>%
  filter(team %in% c(
    "Team DK 1, Travelcare",
    "Team FI 1, Travelcare",
    "Team NO 1, Travelcare",
    "Team SE 1, Travelcare",
    "Team SE 2, Travelcare"
  ))

# ------------------------------------------------------------
# Join + compare
# ------------------------------------------------------------
cmp <- spx_long %>%
  left_join(shift_daily, by = c("team" = "team", "Dato" = "staff_date")) %>%
  rename(date = Dato) %>%
  mutate(
    delta = planned_agents - spx_value,
    ratio = planned_agents / spx_value
  )

readr::write_csv(cmp, file.path(out_dir, "spx_vs_shiftplan_daily.csv"))
readr::write_csv(cmp, file.path(out_dir, "spx_vs_shiftplan_daily_tableau.csv"))

# Write per-team daily comparison under diagnostics folders
purrr::walk(unique(cmp$team), function(tm) {
  team_df <- cmp %>% filter(team == tm)
  team_dir <- file.path(paths$output, "Manning", tm, "diagnostics")
  dir.create(team_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(
    team_df,
    file.path(team_dir, "spx_vs_shiftplan_daily.csv")
  )
  
  team_plot <- team_df %>%
    pivot_longer(cols = c(spx_value, planned_agents),
                 names_to = "series", values_to = "value") %>%
    mutate(
      series = recode(series,
                      spx_value = "SPX (uploadet)",
                      planned_agents = "Planlagte vagter")
    ) %>%
    ggplot(aes(x = date, y = value, color = series)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("SPX (uploadet)" = "#3E3E3E", "Planlagte vagter" = "#D93945")) +
    scale_y_continuous(labels = number_format(accuracy = 0.1)) +
    labs(
      title = paste0("SPX vs planlagte vagter - ", tm),
      x = "Dato",
      y = "Antal",
      color = ""
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ggsave(
    file.path(team_dir, "spx_vs_shiftplan_daily.png"),
    team_plot,
    width = 9,
    height = 4.5,
    dpi = 300
  )
})

# ------------------------------------------------------------
# Plot (facets)
# ------------------------------------------------------------
plot_df <- cmp %>%
  pivot_longer(cols = c(spx_value, planned_agents),
               names_to = "series", values_to = "value") %>%
  mutate(
    series = recode(series,
                    spx_value = "SPX (uploadet)",
                    planned_agents = "Planlagte vagter"),
    team = factor(team, levels = c(
      "Team DK 1, Travelcare",
      "Team FI 1, Travelcare",
      "Team NO 1, Travelcare"
    ))
  )

p <- ggplot(plot_df, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ team, scales = "free_y") +
  scale_color_manual(values = c("SPX (uploadet)" = "#3E3E3E", "Planlagte vagter" = "#D93945")) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title = "SPX vs planlagte vagter pr. dag",
    subtitle = "Sammenligning af SPX‑upload og vores shift plan (sum af agents pr. dag)",
    x = "Dato",
    y = "Antal",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(out_dir, "spx_vs_shiftplan_daily.png"),
  p,
  width = 11,
  height = 6.5,
  dpi = 300
)

message("Saved SPX vs shift plan comparison to: ", out_dir)
