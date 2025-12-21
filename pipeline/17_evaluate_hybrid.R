# ============================================================
# 17_evaluate_hybrid.R
# ------------------------------------------------------------
# Formaal: evaluere hybrid model (GLM + XGB-residual) pr. team.
# Output:
# - <results_base>/final/glm/metrics/metrics_final_glm.(csv|rds) (append)
# - <results_base>/final/glm/plots/hybrid/<team>/*.png
# - <output_base>/baseline_glm/<team>/diagnostics/...
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(xgboost)

source(here("model_functions", "paths.R"))

# ------------------------------------------------------------
# Helpers
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

plot_forecast_vs_actual <- function(df, tm) {
  ggplot(df, aes(ds)) +
    geom_line(aes(y = y, colour = "Actual")) +
    geom_line(aes(y = y_hat, colour = "Hybrid")) +
    scale_colour_manual(values = c("Actual" = "black", "Hybrid" = "blue")) +
    theme_minimal() +
    labs(
      title = paste("Hybrid Forecast –", tm),
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
      title = paste("Hybrid Residuals –", tm),
      y = "Residual"
    )
}

align_xgb_matrix <- function(x, feature_names) {
  missing <- setdiff(feature_names, colnames(x))
  if (length(missing) > 0) {
    add <- matrix(0, nrow = nrow(x), ncol = length(missing))
    colnames(add) <- missing
    x <- cbind(x, add)
  }
  extra <- setdiff(colnames(x), feature_names)
  if (length(extra) > 0) {
    x <- x[, setdiff(colnames(x), extra), drop = FALSE]
  }
  x[, feature_names, drop = FALSE]
}

# ------------------------------------------------------------
# Load data and models
# ------------------------------------------------------------
paths <- get_pipeline_paths()
results_glm_dir     <- file.path(paths$results, "final", "glm")
results_metrics_dir <- file.path(results_glm_dir, "metrics")
results_plots_dir   <- file.path(results_glm_dir, "plots", "hybrid")

output_base_dir    <- file.path(paths$output, "baseline_glm")
output_summary_dir <- file.path(output_base_dir, "_summary")

walk(c(results_metrics_dir, results_plots_dir, output_base_dir, output_summary_dir),
     ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

df_base <- readRDS(here("data_processed", "ts_hourly_all_teams_struct_adj.rds"))
models <- readRDS(here("models", "final_glm_negbin_by_team.rds"))
xgb_resid_by_team <- readRDS(here("models", "final_xgb_residual_by_team.rds"))

train_cutoff <- ymd("2024-12-31")
test_start   <- ymd("2025-01-01")
year_ref     <- 2024

num_vars_other <- c(
  "Juleferie", "Vinterferie", "Påskeferie",
  "Sommerferie", "Efterårsferie"
)

metrics_hyb_list <- list()

for (tm in names(models)) {
  message("---- Evaluating hybrid for team: ", tm)
  mod <- models[[tm]]
  xgb_entry <- xgb_resid_by_team[[tm]]
  if (is.null(mod) || is.null(xgb_entry)) next

  tr <- df_base %>% filter(team == tm, ds < train_cutoff)
  te <- df_base %>% filter(team == tm, ds >= test_start)
  if (nrow(tr) == 0 || nrow(te) == 0) next

  tr <- tr %>%
    mutate(
      hour    = factor(hour),
      weekday = factor(weekday, ordered = FALSE),
      month   = factor(month),
      year_c  = as.numeric(year) - year_ref
    ) %>%
    droplevels()

  te <- te %>%
    mutate(
      hour    = factor(hour,    levels = levels(tr$hour)),
      weekday = factor(weekday, levels = levels(tr$weekday)),
      month   = factor(month,   levels = levels(tr$month)),
      year_c  = as.numeric(year) - year_ref
    )

  tr <- tr %>% mutate(across(all_of(num_vars_other), as.numeric))
  te <- te %>% mutate(across(all_of(num_vars_other), as.numeric))

  y_hat_glm_test <- predict(mod, newdata = te, type = "response")

  fe_test <- te %>%
    transmute(
      hour,
      weekday = factor(weekday, levels = levels(tr$weekday)),
      month   = factor(month,   levels = levels(tr$month)),
      week,
      year,
      Juleferie,
      Vinterferie,
      Påskeferie,
      Sommerferie,
      Efterårsferie
    )

  X_test <- model.matrix(
    ~ hour + weekday + month + week + year +
      Juleferie + Vinterferie + Påskeferie +
      Sommerferie + Efterårsferie - 1,
    data = fe_test
  )

  X_test <- align_xgb_matrix(X_test, xgb_entry$feature_names)
  dtest <- xgb.DMatrix(X_test)
  xgb_model <- xgb.unserialize(xgb_entry$model_raw)
  resid_hat <- predict(xgb_model, dtest)

  y_hat_hyb <- pmax(0, y_hat_glm_test + resid_hat)

  df_plot <- te %>%
    transmute(ds = ds, y = y, y_hat = y_hat_hyb)

  # Plots in results
  out_team_dir <- file.path(results_plots_dir, tm)
  dir.create(out_team_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    file.path(out_team_dir, "forecast_vs_actual.png"),
    plot = plot_forecast_vs_actual(df_plot, tm),
    width = 10, height = 5, dpi = 300
  )

  ggsave(
    file.path(out_team_dir, "residuals.png"),
    plot = plot_residuals(df_plot, tm),
    width = 10, height = 5, dpi = 300
  )

  # Plots in output
  out_diag_dir <- file.path(output_base_dir, tm, "diagnostics")
  out_plot_dir <- file.path(out_diag_dir, "plots", "hybrid")
  dir.create(out_plot_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    file.path(out_plot_dir, "forecast_vs_actual.png"),
    plot = plot_forecast_vs_actual(df_plot, tm),
    width = 10, height = 5, dpi = 300
  )

  ggsave(
    file.path(out_plot_dir, "residuals.png"),
    plot = plot_residuals(df_plot, tm),
    width = 10, height = 5, dpi = 300
  )

  m_hyb <- compute_metrics(df_plot) %>%
    mutate(team = tm, model = "GLM_NegBin_xgb_resid")

  metrics_hyb_list[[tm]] <- m_hyb
}

metrics_hyb <- bind_rows(metrics_hyb_list)

# ------------------------------------------------------------
# Append metrics to results and output
# ------------------------------------------------------------
metrics_path <- file.path(results_metrics_dir, "metrics_final_glm.csv")
if (file.exists(metrics_path)) {
  metrics_existing <- readr::read_csv(metrics_path, show_col_types = FALSE)
  metrics_existing <- metrics_existing %>%
    filter(model != "GLM_NegBin_xgb_resid")
  metrics_final <- bind_rows(metrics_existing, metrics_hyb)
} else {
  metrics_final <- metrics_hyb
}

readr::write_csv(
  metrics_final,
  metrics_path
)

saveRDS(
  metrics_final,
  file.path(results_metrics_dir, "metrics_final_glm.rds")
)

readr::write_csv(
  metrics_final,
  file.path(output_summary_dir, "metrics_final_glm.csv")
)

walk(unique(metrics_final$team), function(tm) {
  out_diag_dir <- file.path(output_base_dir, tm, "diagnostics")
  dir.create(out_diag_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(
    filter(metrics_final, team == tm),
    file.path(out_diag_dir, "metrics_final_glm.csv")
  )
})

message("✔ Hybrid metrics saved to: ", metrics_path)
print(metrics_final)
