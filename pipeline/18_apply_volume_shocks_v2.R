# ============================================================
# 18_apply_volume_shocks_v2.R
# ------------------------------------------------------------
# Formål: anvend volumen-shocks/breakpoints på operationelle forecasts.
# Output: <results_base>/v2/scenarios/fc_operational_scenario_v2.rds med y_hat (justeret).
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))
source(here("model_functions", "se_total.R"))

paths <- get_pipeline_paths()
op_dir <- file.path(paths$results, "v2", "operational")
op_by_team_dir <- file.path(op_dir, "by_team")
scen_by_team_dir <- file.path(paths$results, "v2", "scenarios", "by_team")
dir.create(scen_by_team_dir, recursive = TRUE, showWarnings = FALSE)

shocks_path <- here("config", "volume_shocks.csv")

combined_path <- file.path(op_dir, "fc_operational_raw_v2.rds")
if (!file.exists(combined_path)) {
  stop("Operational forecast file not found: ", combined_path,
       ". Kør 17_operational_forecast_v2.R først.")
}
shocks <- read_csv(shocks_path, show_col_types = FALSE) %>%
  mutate(
    start_date = as_datetime(start_date),
    end_date   = if_else(is.na(end_date) | end_date == "", NA, as_datetime(end_date)),
    priority   = if_else(is.na(priority), 0L, priority)
  ) %>%
  arrange(desc(priority))

apply_shock <- function(fc_df, shock_row) {
  idx <- fc_df$team == shock_row$team &
    fc_df$ds >= shock_row$start_date &
    (is.na(shock_row$end_date) | fc_df$ds < shock_row$end_date) &
    is.na(fc_df$scenario_label)
  
  fc_df$scenario_label[idx] <- shock_row$label
  fc_df$shock_multiplier_applied[idx] <- shock_row$multiplier
  fc_df$y_hat[idx] <- fc_df$y_hat_raw[idx] * shock_row$multiplier
  fc_df
}

fc <- readRDS(combined_path)

# Defensive checks: fail fast with clear message if input is empty/mis-specified
if (nrow(fc) == 0 || ncol(fc) == 0) {
  stop("Operational forecast is empty (", combined_path, "). Kør 17_operational_forecast_v2.R med gyldigt forecast-vindue i config/forecast_v2.json.")
}

if (!"y_hat_raw" %in% names(fc)) {
  stop("Kolonnen 'y_hat_raw' mangler i ", combined_path, ". Tjek 17_operational_forecast_v2.R og upstream data.")
}

if (!all(c("p_se1", "p_se2") %in% names(fc))) {
  # Fixed 50/50 split between SE1 and SE2 (policy decision)
  fc <- ensure_se_share_cols(fc, 0.5, 0.5)
}

fc <- fc %>%
  mutate(
    y_hat = y_hat_raw,
    scenario_label = NA_character_,
    shock_multiplier_applied = 1
  )

for (i in seq_len(nrow(shocks))) {
  shock <- shocks[i, ]
  if (shock$team == "Team SE total") {
    idx_total <- fc$team == "Team SE total" &
      fc$ds >= shock$start_date &
      (is.na(shock$end_date) | fc$ds < shock$end_date) &
      is.na(fc$scenario_label)
    
    if (any(idx_total)) {
      total_shocked <- fc$y_hat_raw[idx_total] * shock$multiplier
      fc$y_hat[idx_total] <- total_shocked
      fc$scenario_label[idx_total] <- shock$label
      fc$shock_multiplier_applied[idx_total] <- shock$multiplier
      
      idx_se <- fc$team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare") &
        fc$ds %in% fc$ds[idx_total] &
        is.na(fc$scenario_label)
      
      fc$y_hat[idx_se & fc$team == "Team SE 1, Travelcare"] <-
        total_shocked[match(fc$ds[idx_se & fc$team == "Team SE 1, Travelcare"], fc$ds[idx_total])] *
        fc$p_se1[idx_se & fc$team == "Team SE 1, Travelcare"]
      
      fc$y_hat[idx_se & fc$team == "Team SE 2, Travelcare"] <-
        total_shocked[match(fc$ds[idx_se & fc$team == "Team SE 2, Travelcare"], fc$ds[idx_total])] *
        fc$p_se2[idx_se & fc$team == "Team SE 2, Travelcare"]
      
      fc$scenario_label[idx_se] <- shock$label
      fc$shock_multiplier_applied[idx_se] <- shock$multiplier
    }
  } else {
    fc <- apply_shock(fc, shock)
  }
}

scen_out_path <- file.path(paths$results, "v2", "scenarios", "fc_operational_scenario_v2.rds")
saveRDS(fc, scen_out_path)
message("✔ Scenarier anvendt. Gemt til: ", scen_out_path)

walk(unique(fc$team), function(tm) {
  out_dir <- file.path(scen_by_team_dir, tm)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, "fc_operational_scenario_v2.rds")
  saveRDS(filter(fc, team == tm), out_path)
  message("✔ Scenarier anvendt. Gemt til: ", out_path)
})
