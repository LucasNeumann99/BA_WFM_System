# ============================================================
# 07_structural_adjustments.R
# ------------------------------------------------------------
# LIGE NU:
# - Ingen ekstra justeringer.
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

# lige nu: ingen regler – bare pass-through
df_adj <- df

saveRDS(df_adj, out_path)

message("✔ Structural adjustments: pass-through (0 regler)")
message("✔ Input:  ts_hourly_all_teams_features.rds")
message("✔ Output: ts_hourly_all_teams_struct_adj.rds")
