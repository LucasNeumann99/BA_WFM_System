#!/usr/bin/env Rscript
# ============================================================
# 27_plot_net_vs_gross_staffing_day.R
# ------------------------------------------------------------
# Figure: Net vs gross staffing profile for a typical weekday.
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

staff <- readr::read_csv(staff_path, show_col_types = FALSE) %>%
  mutate(staff_date = as.Date(staff_date))

weekday_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri")
typical_day <- staff %>%
  filter(weekday %in% weekday_levels) %>%
  summarise(day = min(staff_date)) %>%
  pull(day)

if (is.na(typical_day)) {
  stop("No weekday found in staffing output for ", team_name)
}

day_df <- staff %>%
  filter(staff_date == typical_day) %>%
  mutate(hour = as.integer(hour))

if (!all(c("agents_required", "agents_with_shrinkage") %in% names(day_df))) {
  stop("Staffing file missing required columns: agents_required / agents_with_shrinkage")
}

plot_df <- day_df %>%
  select(hour, agents_required, agents_with_shrinkage) %>%
  pivot_longer(
    cols = c(agents_required, agents_with_shrinkage),
    names_to = "type",
    values_to = "agents"
  ) %>%
  mutate(
    type = recode(
      type,
      agents_required = "Netto (teoretisk behov)",
      agents_with_shrinkage = "Brutto (med shrinkage)"
    )
  )

p <- ggplot(plot_df, aes(x = hour, y = agents, color = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_color_manual(values = c(
    "Netto (teoretisk behov)" = "#3E3E3E",
    "Brutto (med shrinkage)" = "#D93945"
  )) +
  labs(
    title = paste0("Netto vs. brutto bemanding (", team_name, ")"),
    subtitle = paste0("Typisk hverdag: ", typical_day),
    x = "Time på dagen",
    y = "Antal agenter",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(out_dir, "team_dk_netto_vs_brutto_staffing_weekday.png"),
  p,
  width = 10,
  height = 5.5,
  dpi = 300
)

message("Saved: ", file.path(out_dir, "team_dk_netto_vs_brutto_staffing_weekday.png"))
