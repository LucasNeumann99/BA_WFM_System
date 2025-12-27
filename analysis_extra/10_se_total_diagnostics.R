# ============================================================
# 10_se_total_diagnostics.R
# ------------------------------------------------------------
# Formål:
# - Lav diagnostics for Team SE total (SE1+SE2) baseret på final GLM forecast
# - Gem i analysis_extra
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))
paths <- get_pipeline_paths()

fc <- readRDS(file.path(paths$results, "final", "glm", "fc_final_glm_negbin.rds"))

se_df <- fc %>%
  filter(team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare"))

se_total <- se_df %>%
  group_by(ds) %>%
  summarise(
    y = sum(y, na.rm = TRUE),
    y_hat = sum(y_hat, na.rm = TRUE),
    .groups = "drop"
  )

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
  mutate(team = "Team SE total", model = "AGG_SE1_SE2")

out_dir <- file.path(paths$output, "analysis_extra", "Team SE total diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(metrics, file.path(out_dir, "metrics_team_se_total.csv"))

p <- ggplot(se_total, aes(ds)) +
  geom_line(aes(y = y, colour = "Actual")) +
  geom_line(aes(y = y_hat, colour = "Forecast")) +
  scale_colour_manual(values = c(
    "Actual" = "#3E3E3E",
    "Forecast" = "#D93945"
  )) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Team SE total – Forecast vs Actual",
    y = "Calls",
    colour = ""
  )

ggsave(
  filename = file.path(out_dir, "Team_SE_total_forecast_vs_actual.png"),
  plot = p,
  width = 10, height = 5, dpi = 300
)

message("✔ Saved: ", file.path(out_dir, "metrics_team_se_total.csv"))
message("✔ Saved: ", file.path(out_dir, "Team_SE_total_forecast_vs_actual.png"))
