# ============================================================
# 22_operational_diagnostics_v2.R
# ------------------------------------------------------------
# Formål:
# - Lav diagnostics for den operative forecast (post shocks)
# - Gem kun operative plots/metrics i output/diagnostics/<team>
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)

source(here("model_functions", "paths.R"))

cfg <- fromJSON(here("config", "forecast_v2.json"))
tz_info <- if (!is.null(cfg$timezone) && nzchar(cfg$timezone)) cfg$timezone else "UTC"
eval_cfg <- if (!is.null(cfg$evaluation)) cfg$evaluation else list()

test_start_val <- if (!is.null(eval_cfg$test_start) && nzchar(eval_cfg$test_start)) {
  eval_cfg$test_start
} else {
  "2025-01-01 00:00:00"
}
test_end_val <- if (!is.null(eval_cfg$test_end) && nzchar(eval_cfg$test_end)) {
  eval_cfg$test_end
} else {
  ""
}

test_start_dt <- ymd_hms(test_start_val, tz = tz_info)
test_end_dt <- ymd_hms(test_end_val, tz = tz_info)

paths <- get_pipeline_paths()
op_scen_path <- file.path(paths$results, "v2", "scenarios", "fc_operational_scenario_v2.rds")
df_actuals_path <- here("data_processed", "ts_hourly_all_teams_struct_adj.rds")

if (!file.exists(op_scen_path)) {
  stop("Operational scenario forecast not found: ", op_scen_path,
       ". Kør 18_apply_volume_shocks_v2.R først.")
}

fc <- readRDS(op_scen_path)
df_actuals <- readRDS(df_actuals_path)

if (is.na(test_end_dt)) {
  test_end_dt <- max(df_actuals$ds)
}

fc <- fc %>%
  mutate(y_hat = if ("y_hat" %in% names(.)) y_hat else y_hat_raw) %>%
  filter(ds >= test_start_dt, ds <= test_end_dt)

actuals <- df_actuals %>%
  filter(ds >= test_start_dt, ds <= test_end_dt) %>%
  select(team, ds, y)

df <- fc %>%
  left_join(actuals, by = c("team", "ds")) %>%
  filter(team != "Team SE total")

if (nrow(df) == 0) {
  stop("Ingen overlap mellem operational forecast og actuals i test-perioden. ",
       "Tjek evaluation test_start/test_end i config/forecast_v2.json.")
}

plot_colors <- c(
  "Actual" = "#3E3E3E",
  "Forecast" = "#D93945"
)

compute_metrics <- function(df_in) {
  df_in %>%
    summarise(
      n = sum(!is.na(y_hat) & !is.na(y)),
      RMSE = sqrt(mean((y - y_hat)^2, na.rm = TRUE)),
      MAE  = mean(abs(y - y_hat), na.rm = TRUE),
      MAPE = mean(abs(y - y_hat) / pmax(y, 1), na.rm = TRUE),
      SMAPE = mean(abs(y - y_hat) / ((abs(y) + abs(y_hat)) / 2), na.rm = TRUE),
      Bias_mean = mean(y_hat - y, na.rm = TRUE),
      Bias_sd   = sd(y_hat - y, na.rm = TRUE)
    )
}

base_out_dir <- file.path(paths$output, "diagnostics")
metrics_all <- list()

for (tm in unique(df$team)) {
  df_team <- df %>% filter(team == tm)
  if (nrow(df_team) == 0) next
  
  diag_dir <- file.path(base_out_dir, tm)
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  
  p_fc <- ggplot(df_team, aes(ds)) +
    geom_line(aes(y = y, colour = "Actual")) +
    geom_line(aes(y = y_hat, colour = "Forecast")) +
    scale_colour_manual(values = plot_colors) +
    theme_minimal(base_size = 11) +
    labs(
      title = paste("Operativ forecast vs actual –", tm),
      y = "Calls",
      colour = ""
    )
  
  ggsave(
    file.path(diag_dir, "forecast_vs_actual.png"),
    plot = p_fc,
    width = 10, height = 5, dpi = 300
  )
  
  p_res <- ggplot(df_team %>% mutate(resid = y - y_hat), aes(ds, resid)) +
    geom_line(color = plot_colors[["Actual"]]) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal(base_size = 11) +
    labs(
      title = paste("Operative residuals –", tm),
      y = "Residual"
    )
  
  ggsave(
    file.path(diag_dir, "residuals.png"),
    plot = p_res,
    width = 10, height = 5, dpi = 300
  )
  
  m <- compute_metrics(df_team) %>%
    mutate(team = tm, model = "OPERATIVE")
  
  readr::write_csv(m, file.path(diag_dir, "metrics_operational.csv"))
  metrics_all[[tm]] <- m
  
}

metrics_final <- bind_rows(metrics_all)
total_dir <- file.path(base_out_dir, "Total_Travelcare")
dir.create(total_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(metrics_final, file.path(total_dir, "metrics_operational.csv"))

message("✔ Operational diagnostics saved under: ", base_out_dir)
