# ============================================================
# 21_export_erlang_input_v2.R
# ------------------------------------------------------------
# Formål: eksportér volumen til Erlang C fra scenarie-justerede forecasts.
# Output: <output_base>/v2/erlang/erlang_input_v2.csv med team, ds, volume (= y_hat).
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)

source(here("model_functions", "paths.R"))

# Standardparametre til Erlang C (kan overskrives hvis de findes i data)
AHT_SEC       <- 316      # gennemsnitlig AHT i sek
TARGET_SL     <- 0.80     # mål for service level (fx 80 %)
THRESHOLD_SEC <- 25       # SLA-grænse i sek
SHRINKAGE     <- 0.35     # forventet shrinkage (fx 35 %)

cfg <- fromJSON(here("config", "forecast_v2.json"))
tz_info <- cfg$timezone %||% "UTC"
op_cfg  <- cfg$operational

forecast_start <- ymd_hms(op_cfg$forecast_start, tz = tz_info)
forecast_end   <- ymd_hms(op_cfg$forecast_end,   tz = tz_info)

paths <- get_pipeline_paths()
base_out_dir <- file.path(paths$output, "baseline_glm")
dir.create(base_out_dir, recursive = TRUE, showWarnings = FALSE)

fc_path <- file.path(paths$results, "v2", "scenarios", "fc_operational_scenario_v2.rds")

fc <- readRDS(fc_path) %>%
  filter(ds >= forecast_start, ds <= forecast_end)

# Sikr kolonner: brug eksisterende, ellers default NA
ensure_col <- function(df, col, default = NA) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}

fc <- fc %>%
  ensure_col("model_used", "unknown") %>%
  ensure_col("run_id", NA_character_) %>%
  ensure_col("aht_sec", NA_real_) %>%
  ensure_col("target_sl", NA_real_) %>%
  ensure_col("threshold_sec", NA_real_) %>%
  ensure_col("shrinkage", NA_real_) %>%
  mutate(
    calls = if ("volume" %in% names(.)) volume else y_hat,
    calls = if_else(is.finite(calls), calls, 0),
    calls = replace_na(calls, 0),
    aht_sec       = coalesce(aht_sec, AHT_SEC),
    target_sl     = coalesce(target_sl, TARGET_SL),
    threshold_sec = coalesce(threshold_sec, THRESHOLD_SEC),
    shrinkage     = coalesce(shrinkage, SHRINKAGE)
  )

erlang_input <- fc %>%
  transmute(
    team,
    ds,
    calls,
    aht_sec,
    target_sl,
    threshold_sec,
    shrinkage,
    model_used,
    scenario_label,
    run_id
  )

teams <- sort(unique(erlang_input$team))
for (tm in teams) {
  erlang_dir <- file.path(base_out_dir, tm, "erlang")
  dir.create(erlang_dir, recursive = TRUE, showWarnings = FALSE)
  out_csv <- file.path(erlang_dir, "erlang_input_v2.csv")
  write_csv(filter(erlang_input, team == tm), out_csv)
  message("✔ Erlang input gemt: ", out_csv)
}
