# ============================================================
# 10_se_total_diagnostics.R
# ------------------------------------------------------------
# Formål:
# - Lav diagnostics for Team SE total (SE1+SE2) baseret på operational forecast
# - Gem i diagnostics/Team SE total
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)

source(here("model_functions", "paths.R"))
paths <- get_pipeline_paths()

cfg <- fromJSON(here("config", "forecast_v2.json"))
tz_info <- cfg$timezone %||% "UTC"
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

fc <- readRDS(file.path(paths$results, "v2", "scenarios", "fc_operational_scenario_v2.rds"))
df_actuals <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))

if (is.na(test_end_dt)) {
  test_end_dt <- max(df_actuals$ds)
}

se_df <- fc %>%
  filter(team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare")) %>%
  filter(ds >= test_start_dt, ds <= test_end_dt) %>%
  mutate(y_hat = if ("y_hat" %in% names(.)) y_hat else y_hat_raw)

se_actuals <- df_actuals %>%
  filter(team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare")) %>%
  filter(ds >= test_start_dt, ds <= test_end_dt) %>%
  group_by(ds) %>%
  summarise(y = sum(y, na.rm = TRUE), .groups = "drop")

se_total <- se_df %>%
  group_by(ds) %>%
  summarise(
    y_hat = sum(y_hat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(se_actuals, by = "ds")

compute_metrics <- function(df) {
  df %>%
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

metrics <- compute_metrics(se_total) %>%
  mutate(team = "Team SE total", model = "OPERATIVE_SE_TOTAL")

out_dir <- file.path(paths$output, "diagnostics", "Team SE total")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(metrics, file.path(out_dir, "metrics_operational.csv"))

p <- ggplot(se_total, aes(ds)) +
  geom_line(aes(y = y, colour = "Actual")) +
  geom_line(aes(y = y_hat, colour = "Forecast")) +
  scale_colour_manual(values = c(
    "Actual" = "#3E3E3E",
    "Forecast" = "#D93945"
  )) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Team SE total – Forecast vs Actual (operativ)",
    y = "Calls",
    colour = ""
  )

ggsave(
  filename = file.path(out_dir, "forecast_vs_actual.png"),
  plot = p,
  width = 10, height = 5, dpi = 300
)

p_res <- ggplot(se_total %>% mutate(resid = y - y_hat), aes(ds, resid)) +
  geom_line(color = "#3E3E3E") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Team SE total – Residualer (operativ)",
    y = "Residual"
  )

ggsave(
  filename = file.path(out_dir, "residuals.png"),
  plot = p_res,
  width = 10, height = 5, dpi = 300
)

split_plot_src <- file.path(
  paths$output,
  "analysis_extra",
  "Strukturelle ændringer",
  "Team_se_routing_problem",
  "se_actual_share_last6m.png"
)
if (file.exists(split_plot_src)) {
  file.copy(
    split_plot_src,
    file.path(out_dir, "routing_split_key.png"),
    overwrite = TRUE
  )
}

message("✔ Saved: ", file.path(out_dir, "metrics_operational.csv"))
message("✔ Saved: ", file.path(out_dir, "forecast_vs_actual.png"))
message("✔ Saved: ", file.path(out_dir, "residuals.png"))
