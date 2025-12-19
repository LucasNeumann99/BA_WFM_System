# ============================================================
# 19_export_erlang_input_v2.R
# ------------------------------------------------------------
# Formål: eksportér volumen til Erlang C fra scenarie-justerede forecasts.
# Output: output/v2/erlang/erlang_input_v2.csv med team, ds, volume (= y_hat).
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)

cfg <- fromJSON(here("config", "forecast_v2.json"))
tz_info <- cfg$timezone %||% "UTC"
op_cfg  <- cfg$operational

forecast_start <- ymd_hms(op_cfg$forecast_start, tz = tz_info)
forecast_end   <- ymd_hms(op_cfg$forecast_end,   tz = tz_info)

dir.create(here("output", "v2", "erlang"), recursive = TRUE, showWarnings = FALSE)

fc_path <- here("results", "v2", "scenarios", "fc_operational_scenario_v2.rds")
out_csv <- here("output", "v2", "erlang", "erlang_input_v2.csv")

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
    calls = if ("volume" %in% names(.)) volume else y_hat
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

write_csv(erlang_input, out_csv)
message("✔ Erlang input gemt: ", out_csv)
