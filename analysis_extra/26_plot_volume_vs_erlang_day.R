#!/usr/bin/env Rscript
# ============================================================
# 26_plot_volume_vs_erlang_day.R
# ------------------------------------------------------------
# Figure: Daily forecast volume vs Erlang load for a winter holiday day (Team DK)
# Output: BA_WFM_Output/analysis_extra/Y_præsentation
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(scales)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "Y_præsentation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

team_dk <- "Team DK 1, Travelcare"

features <- readRDS(here("data_processed", "ts_future_all_teams_features.rds")) %>%
  filter(team == team_dk)

erlang_path <- file.path(
  paths$output,
  "Manning",
  team_dk,
  "erlang",
  "erlang_output_v2.csv"
)

erlang <- readr::read_csv(erlang_path, show_col_types = FALSE) %>%
  mutate(ds = as.POSIXct(ds, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
  filter(!is.na(ds))

range_min <- min(erlang$ds, na.rm = TRUE)
range_max <- max(erlang$ds, na.rm = TRUE)

winter_day <- features %>%
  filter(ds >= range_min, ds <= range_max, Vinterferie == 1) %>%
  summarise(day = min(as_date(ds))) %>%
  pull(day)

if (is.na(winter_day)) {
  stop("No Vinterferie day found for Team DK within Erlang output range.")
}

day_df <- erlang %>%
  filter(as_date(ds) == winter_day) %>%
  mutate(hour = hour(ds))

if (nrow(day_df) == 0) {
  stop("No Erlang output found for winter holiday day: ", winter_day)
}

scale_factor <- max(day_df$calls, na.rm = TRUE) / max(day_df$traffic_erlangs, na.rm = TRUE)

p <- ggplot(day_df, aes(x = hour)) +
  geom_line(aes(y = traffic_erlangs * scale_factor, color = "Erlang load"),
            linewidth = 1.1, alpha = 0.7, linetype = "dashed") +
  geom_line(aes(y = calls, color = "Forecast calls"), linewidth = 1.3) +
  geom_point(aes(y = calls, color = "Forecast calls"), size = 1.6) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_y_continuous(
    labels = comma,
    name = "Forecast calls",
    sec.axis = sec_axis(~ . / scale_factor, name = "Traffic (Erlangs)")
  ) +
  scale_color_manual(
    values = c("Forecast calls" = "#D93945", "Erlang load" = "#3E3E3E")
  ) +
  labs(
    title = paste0("Sammenhæng mellem forecast-volumen og Erlang load (", winter_day, ")"),
    subtitle = "Team DK 1, Travelcare - vinterferieeksempel",
    x = "Time på dagen",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(out_dir, "team_dk_forecast_vs_erlang_winter_day.png"),
  p,
  width = 10,
  height = 5.5,
  dpi = 300
)

message("Saved: ", file.path(out_dir, "team_dk_forecast_vs_erlang_winter_day.png"))
