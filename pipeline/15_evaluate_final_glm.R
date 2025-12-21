# ============================================================
# 15_evaluate_final_glm.R
# ------------------------------------------------------------
# Formaal: evaluere endelig GLM pr. team (baseline), gemme plots og metrics.
# Output:
# - <results_base>/final/glm/metrics/metrics_final_glm.(csv|rds)
# - <results_base>/final/glm/summaries/model_summary_final_glm*.csv
# - <results_base>/final/glm/plots/glm/<team>/*.png
# - <output_base>/baseline_glm/<team>/diagnostics/...
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(broom)

source(here("model_functions", "paths.R"))

# ------------------------------------------------------------
# Load forecasts
# ------------------------------------------------------------
paths <- get_pipeline_paths()
fc_path <- file.path(paths$results, "final", "glm", "fc_final_glm_negbin.rds")
fc <- readRDS(fc_path)

teams <- unique(fc$team)

# ------------------------------------------------------------
# Directory setup
# ------------------------------------------------------------
results_glm_dir     <- file.path(paths$results, "final", "glm")
results_metrics_dir <- file.path(results_glm_dir, "metrics")
results_summary_dir <- file.path(results_glm_dir, "summaries")
results_plots_dir   <- file.path(results_glm_dir, "plots", "glm")

output_base_dir    <- file.path(paths$output, "baseline_glm")
output_summary_dir <- file.path(output_base_dir, "_summary")

walk(c(results_metrics_dir, results_summary_dir, results_plots_dir,
       output_base_dir, output_summary_dir),
     ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

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
    geom_line(aes(y = y_hat, colour = "GLM")) +
    scale_colour_manual(values = c("Actual" = "black", "GLM" = "red")) +
    theme_minimal() +
    labs(
      title = paste("Final GLM Forecast –", tm),
      y = "Calls",
      colour = ""
    )
}

plot_residuals <- function(df, tm) {
  ggplot(df %>% mutate(resid = y - y_hat), aes(ds, resid)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = paste("Residuals –", tm),
      y = "Residual"
    )
}

# ------------------------------------------------------------
# Evaluate per team
# ------------------------------------------------------------
metrics_all <- list()

for (tm in teams) {
  message("---- Plotting: ", tm)

  df <- fc %>% filter(team == tm)
  model_tag <- if ("model_used" %in% names(df)) unique(df$model_used) else "GLM_NegBin_baseline"
  if (length(model_tag) != 1) model_tag <- "GLM_NegBin_baseline"

  # Plots in results
  out_team_dir <- file.path(results_plots_dir, tm)
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

  # Plots in output
  out_diag_dir <- file.path(output_base_dir, tm, "diagnostics")
  out_plot_dir <- file.path(out_diag_dir, "plots", "glm")
  dir.create(out_plot_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    file.path(out_plot_dir, "forecast_vs_actual.png"),
    plot = plot_forecast_vs_actual(df, tm),
    width = 10, height = 5, dpi = 300
  )

  ggsave(
    file.path(out_plot_dir, "residuals.png"),
    plot = plot_residuals(df, tm),
    width = 10, height = 5, dpi = 300
  )

  # Metrics
  m <- compute_metrics(df) %>%
    mutate(team = tm, model = model_tag)

  metrics_all[[tm]] <- m
}

metrics_glm <- bind_rows(metrics_all)

# ------------------------------------------------------------
# Save metrics (results + output)
# ------------------------------------------------------------
readr::write_csv(
  metrics_glm,
  file.path(results_metrics_dir, "metrics_final_glm.csv")
)

saveRDS(
  metrics_glm,
  file.path(results_metrics_dir, "metrics_final_glm.rds")
)

readr::write_csv(
  metrics_glm,
  file.path(output_summary_dir, "metrics_final_glm.csv")
)

walk(unique(metrics_glm$team), function(tm) {
  out_diag_dir <- file.path(output_base_dir, tm, "diagnostics")
  dir.create(out_diag_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(
    filter(metrics_glm, team == tm),
    file.path(out_diag_dir, "metrics_final_glm.csv")
  )
})

# ------------------------------------------------------------
# Model summary (coefficients) to CSV
# ------------------------------------------------------------
models <- readRDS(here("models", "final_glm_negbin_by_team.rds"))
model_summary <- imap_dfr(models, ~ broom::tidy(.x) %>% mutate(team = .y))

readr::write_csv(
  model_summary,
  file.path(results_summary_dir, "model_summary_final_glm.csv")
)

readr::write_csv(
  model_summary,
  file.path(output_summary_dir, "model_summary_final_glm.csv")
)

slugify <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

walk(unique(model_summary$team), function(tm) {
  readr::write_csv(
    filter(model_summary, team == tm),
    file.path(results_summary_dir, paste0("model_summary_final_glm_", slugify(tm), ".csv"))
  )

  readr::write_csv(
    filter(model_summary, team == tm),
    file.path(output_base_dir, tm, "diagnostics", "model_summary_final_glm.csv")
  )
})

message("✔ Plotting complete.")
message("✔ Metrics saved to: ")
message("  - CSV: ", file.path(results_metrics_dir, "metrics_final_glm.csv"))
message("  - RDS: ", file.path(results_metrics_dir, "metrics_final_glm.rds"))
print(metrics_glm)
