# ============================================================
# plot_team_no_level_adjustment.R
# ------------------------------------------------------------
# Formål:
# - Visualisere rå vs justeret historik for Team NO (dagligt)
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

# ------------------------------------------------------------
# 1) Load data
# ------------------------------------------------------------
df_raw <- readRDS(here("data_processed", "ts_hourly_all_teams_struct.rds"))
df_adj <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))

team_name <- "Team NO 1, Travelcare"
cut_date <- ymd("2025-06-01")

df_raw_team <- df_raw %>%
  filter(team == team_name)

df_adj_team <- df_adj %>%
  filter(team == team_name)

# ------------------------------------------------------------
# 2) Daily aggregation
# ------------------------------------------------------------
raw_daily <- df_raw_team %>%
  mutate(day = as_date(ds)) %>%
  group_by(day) %>%
  summarise(daily_y = sum(y, na.rm = TRUE), .groups = "drop") %>%
  mutate(version = "raw")

adj_daily <- df_adj_team %>%
  mutate(day = as_date(ds)) %>%
  group_by(day) %>%
  summarise(daily_y = sum(y, na.rm = TRUE), .groups = "drop") %>%
  mutate(version = "adjusted")

df_plot <- bind_rows(raw_daily, adj_daily)

# ------------------------------------------------------------
# 3) Plot
# ------------------------------------------------------------
plot_colors <- c(
  "raw" = "#3E3E3E",
  "adjusted" = "#D93945"
)

p <- ggplot(df_plot, aes(x = day, y = daily_y, color = version)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.numeric(cut_date), linetype = "dashed") +
  scale_color_manual(values = plot_colors) +
  scale_x_date(limits = c(ymd("2024-01-01"), ymd("2025-12-31"))) +
  labs(
    title = "Team NO – daglige opkald, før og efter niveauskalering",
    x = "Dato",
    y = "Daglige opkald",
    color = "",
    caption = "Justeret serie er skaleret ned før 2025-06-01."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# ------------------------------------------------------------
# 4) Save
# ------------------------------------------------------------
source(here("model_functions", "paths.R"))
paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(out_dir, "team_no_level_adjustment.png"),
  plot = p,
  width = 10,
  height = 5,
  dpi = 300
)

message("✔ Saved: ", file.path(out_dir, "team_no_level_adjustment.png"))
