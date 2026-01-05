#!/usr/bin/env Rscript
# ============================================================
# 30_plot_staffing_vs_required_weekday.R
# ------------------------------------------------------------
# Plot: Required (Erlang) vs staffed (shift plan) for a typical weekday.
# Output: BA_WFM_Output/analysis_extra/Y_præsentation
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "Y_præsentation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

team_name <- "Team DK 1, Travelcare"
staff_path <- file.path(
  paths$output,
  "Manning",
  team_name,
  "staffing",
  "hourly_coverage_vs_required_v2.csv"
)

df <- readr::read_csv(staff_path, show_col_types = FALSE) %>%
  mutate(staff_date = as.Date(staff_date))

weekday_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri")
typical_day <- df %>%
  filter(weekday %in% weekday_levels) %>%
  summarise(day = min(staff_date)) %>%
  pull(day)

if (is.na(typical_day)) {
  stop("No weekday found in staffing output for ", team_name)
}

day_df <- df %>%
  filter(staff_date == typical_day) %>%
  mutate(hour = as.integer(hour))

plot_df <- day_df %>%
  select(hour, staffed_agents, agents_with_shrinkage) %>%
  pivot_longer(
    cols = c(agents_with_shrinkage, staffed_agents),
    names_to = "series",
    values_to = "agents"
  ) %>%
  mutate(
    series = recode(series,
                    agents_with_shrinkage = "Bruttokrav (Erlang)",
                    staffed_agents = "Faktisk bemanding")
  )

p <- ggplot(plot_df, aes(x = hour, y = agents, color = series)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_color_manual(values = c(
    "Bruttokrav (Erlang)" = "#3E3E3E",
    "Faktisk bemanding" = "#D93945"
  )) +
  labs(
    title = "Bruttokrav vs. faktisk bemanding (typisk hverdag)",
    subtitle = paste0(team_name, " - ", typical_day),
    x = "Time på dagen",
    y = "Antal agenter",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(out_dir, "team_dk_required_vs_staffed_weekday.png"),
  p,
  width = 10,
  height = 5.5,
  dpi = 300
)

message("Saved: ", file.path(out_dir, "team_dk_required_vs_staffed_weekday.png"))
