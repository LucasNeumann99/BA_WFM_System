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
op_by_team_dir <- file.path(paths$results, "v2", "operational", "by_team")
scen_by_team_dir <- file.path(paths$results, "v2", "scenarios", "by_team")
dir.create(scen_by_team_dir, recursive = TRUE, showWarnings = FALSE)

shocks_path <- here("config", "volume_shocks.csv")

fc_files <- list.files(
  op_by_team_dir,
  pattern = "fc_operational_raw_v2.rds",
  recursive = TRUE,
  full.names = TRUE
)
if (length(fc_files) == 0) {
  stop("Operational forecast files not found under: ", op_by_team_dir,
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

for (fc_path in fc_files) {
  fc <- readRDS(fc_path)
  
  # Defensive checks: fail fast with clear message if input is empty/mis-specified
  if (nrow(fc) == 0 || ncol(fc) == 0) {
    stop("Operational forecast is empty (", fc_path, "). Kør 17_operational_forecast_v2.R med gyldigt forecast-vindue i config/forecast_v2.json.")
  }
  
  if (!"y_hat_raw" %in% names(fc)) {
    stop("Kolonnen 'y_hat_raw' mangler i ", fc_path, ". Tjek 17_operational_forecast_v2.R og upstream data.")
  }
  
  fc <- fc %>%
    mutate(
      y_hat = y_hat_raw,
      scenario_label = NA_character_,
      shock_multiplier_applied = 1
    )
  
  for (i in seq_len(nrow(shocks))) {
    fc <- apply_shock(fc, shocks[i, ])
  }
  
  team_name <- basename(dirname(fc_path))
  out_dir <- file.path(scen_by_team_dir, team_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, "fc_operational_scenario_v2.rds")
  saveRDS(fc, out_path)
  message("✔ Scenarier anvendt. Gemt til: ", out_path)
}
