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

paths <- get_pipeline_paths()
scenarios_dir <- file.path(paths$results, "v2", "scenarios")
dir.create(scenarios_dir, recursive = TRUE, showWarnings = FALSE)

fc_raw_path   <- file.path(paths$results, "v2", "operational", "fc_operational_raw_v2.rds")
shocks_path   <- here("config", "volume_shocks.csv")
fc_out_path   <- file.path(scenarios_dir, "fc_operational_scenario_v2.rds")

fc <- readRDS(fc_raw_path)
shocks <- read_csv(shocks_path, show_col_types = FALSE) %>%
  mutate(
    start_date = as_datetime(start_date),
    end_date   = if_else(is.na(end_date) | end_date == "", NA, as_datetime(end_date)),
    priority   = if_else(is.na(priority), 0L, priority)
  ) %>%
  arrange(desc(priority))

# Defensive checks: fail fast with clear message if input is empty/mis-specified
if (nrow(fc) == 0 || ncol(fc) == 0) {
  stop("Operational forecast is empty (", fc_raw_path, "). Kør 17_operational_forecast_v2.R med gyldigt forecast-vindue i config/forecast_v2.json.")
}

if (!"y_hat_raw" %in% names(fc)) {
  stop("Kolonnen 'y_hat_raw' mangler i ", fc_raw_path, ". Tjek 17_operational_forecast_v2.R og upstream data.")
}

fc <- fc %>%
  mutate(
    y_hat = y_hat_raw,
    scenario_label = NA_character_,
    shock_multiplier_applied = 1
  )

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

for (i in seq_len(nrow(shocks))) {
  fc <- apply_shock(fc, shocks[i, ])
}

saveRDS(fc, fc_out_path)
message("✔ Scenarier anvendt. Gemt til: ", fc_out_path)
