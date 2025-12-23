# ============================================================
# 07_structural_adjustments.R
# ------------------------------------------------------------
# LIGE NU:
# - Structural level shift
# - Storebrand (C370) er allerede filtreret ud i 03_aggregate_hourly.R
#
# Input:
#   data_processed/ts_hourly_all_teams_features.rds  (fra 06)
# Output:
#   data_processed/ts_hourly_all_teams_struct_adj.rds
# ============================================================

library(tidyverse)
library(here)

in_path  <- here("data_processed", "ts_hourly_all_teams_features.rds")
out_path <- here("data_processed", "ts_hourly_all_teams_struct_adj.rds")

df <- readRDS(in_path)

message("Rows before structural adjustments: ", nrow(df))

# ------------------------------------------------------------
# Team NO: structural level shift (post Storebrand exit)
# ------------------------------------------------------------
team_no    <- "Team NO 1, Travelcare"
break_date <- as.Date("2025-06-01")

pre_start  <- as.Date("2024-06-01")
pre_end    <- as.Date("2024-11-30")
post_start <- as.Date("2025-06-01")
post_end   <- as.Date("2025-11-30")

df_no <- df %>%
  filter(team == team_no) %>%
  mutate(date = as.Date(ds))

pre_mean <- df_no %>%
  filter(date >= pre_start, date <= pre_end) %>%
  summarise(mean_y = mean(y, na.rm = TRUE)) %>%
  pull(mean_y)

post_mean <- df_no %>%
  filter(date >= post_start, date <= post_end) %>%
  summarise(mean_y = mean(y, na.rm = TRUE)) %>%
  pull(mean_y)

applied <- FALSE
if (is.na(pre_mean) || is.na(post_mean) || pre_mean <= 0) {
  warning("Team NO structural adjustment skipped (insufficient data for ratio).")
  df_adj <- df
} else {
  level_factor <- post_mean / pre_mean
  message("Team NO level factor (", format(pre_start), "–", format(pre_end), " vs ",
          format(post_start), "–", format(post_end), "): ",
          round(level_factor, 4))
  
  df_adj <- df %>%
    mutate(
      y = if_else(team == team_no & as.Date(ds) < break_date, y * level_factor, y)
    )
  applied <- TRUE
}

saveRDS(df_adj, out_path)

message("✔ Structural adjustments: ",
        if (applied) "Team NO level shift applied" else "pass-through (0 regler)")
message("✔ Input:  ts_hourly_all_teams_features.rds")
message("✔ Output: ts_hourly_all_teams_struct_adj.rds")
