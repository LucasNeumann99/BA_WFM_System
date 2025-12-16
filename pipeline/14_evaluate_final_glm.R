# ============================================================
# 14_evaluate_final_glm.R
# ------------------------------------------------------------
# Evaluates FINAL NegBin GLM baseline
# - Produces 2 plots per team:
#     1) Forecast vs actual
#     2) Residuals over time
# - Saves metrics in:
#     output/diagnostics/metrics_final_glm.csv
# - Saves plots in:
#     figures/final/glm/<team>/
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

# ------------------------------------------------------------
# Load forecasts
# ------------------------------------------------------------
fc_path <- here("results", "final", "glm", "fc_final_glm_negbin.rds")
fc <- readRDS(fc_path)

teams <- unique(fc$team)

# ------------------------------------------------------------
# Directory setup
# ------------------------------------------------------------
metrics_dir <- here("output", "diagnostics")
fig_dir     <- here("figures", "final", "glm")

dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Filter to operational hours (07–22)
# ------------------------------------------------------------
fc <- fc %>%
  mutate(hour = as.numeric(hour)) %>%
  filter(hour >= 7, hour <= 22)

# ------------------------------------------------------------
# Metric functions
# ------------------------------------------------------------
compute_metrics <- function(df) {
  df %>%
    summarise(
      n = sum(!is.na(y_hat) & !is.na(y)),
      RMSE = sqrt(mean((y - y_hat)^2)),
      MAE  = mean(abs(y - y_hat)),
      MAPE = mean(abs(y - y_hat) / pmax(y, 1)),
      SMAPE = mean(abs(y - y_hat) / ((abs(y) + abs(y_hat)) / 2)),
      Bias_mean = mean(y_hat - y),
      Bias_sd   = sd(y_hat - y)
    )
}

# ------------------------------------------------------------
# Plot functions
# ------------------------------------------------------------
plot_forecast_vs_actual <- function(df, tm) {
  ggplot(df, aes(ds)) +
    geom_line(aes(y = y, colour = "Actual")) +
    geom_line(aes(y = y_hat, colour = "NegBin")) +
    scale_colour_manual(values = c("Actual" = "black", "NegBin" = "red")) +
    theme_minimal() +
    labs(title = paste("Final GLM Forecast –", tm), y = "Calls", colour = "")
}

plot_residuals <- function(df, tm) {
  ggplot(df %>% mutate(resid = y - y_hat), aes(ds, resid)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("Residuals –", tm), y = "Residual")
}

# ------------------------------------------------------------
# Evaluate per team
# ------------------------------------------------------------
metrics_all <- list()

for (tm in teams) {
  
  message("---- Plotting: ", tm)
  
  df <- fc %>% filter(team == tm)
  
  # Save plots
  out_team_dir <- file.path(fig_dir, tm)
  dir.create(out_team_dir, recursive = TRUE, showWarnings = FALSE)
  
  ggsave(
    file.path(out_team_dir, "forecast_vs_actual.png"),
    plot = plot_forecast_vs_actual(df, tm),
    width = 10, height = 5, dpi = 300
  )
  
  ggsave(
    file.path(out_team_dir, "residuals.png"),
    plot = plot_residuals(df, tm),
    width = 10, height = 5, dpi = 300
  )
  
  # Metrics
  m <- compute_metrics(df) %>%
    mutate(team = tm, model = "GLM_NegBin")
  
  metrics_all[[tm]] <- m
}

# ------------------------------------------------------------
# Save metrics (CSV = BA / report)
# ------------------------------------------------------------
metrics_final <- bind_rows(metrics_all)

write_csv(
  metrics_final,
  file.path(metrics_dir, "metrics_final_glm.csv")
)

message("✔ Plotting complete.")
message("✔ Metrics saved to: output/diagnostics/metrics_final_glm.csv")
print(metrics_final)

