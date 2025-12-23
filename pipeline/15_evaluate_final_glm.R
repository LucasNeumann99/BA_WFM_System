# ============================================================
# 15_evaluate_final_glm.R
# ------------------------------------------------------------
# Formål: evaluere endelig GLM pr. team (lag eller baseline), gemme plots og metrics til rapport/teknik.
# - Produces 2 plots per team:
#     1) Forecast vs actual
#     2) Residuals over time
# - Saves metrics as:
#     - CSV (til BA/rapport): <output_base>/diagnostics/metrics_final_glm.csv
#     - RDS (til modeller/teknik): <results_base>/final/glm/metrics_final_glm.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(broom)

source(here("model_functions", "paths.R"))

# ------------------------------------------------------------
# Plot palette
# ------------------------------------------------------------
plot_colors <- c(
  "Actual" = "#3E3E3E",  # Gray_dark
  "NegBin" = "#D93945"   # SOS_red
)

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
metrics_base_dir <- file.path(paths$output, "baseline_glm")
metrics_results_dir <- file.path(paths$results, "final", "glm")

dir.create(metrics_base_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(metrics_results_dir, recursive = TRUE, showWarnings = FALSE)

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
    scale_colour_manual(values = plot_colors) +
    theme_minimal() +
    labs(
      title = paste("Final GLM Forecast –", tm),
      y = "Calls",
      colour = ""
    )
}

plot_residuals <- function(df, tm) {
  ggplot(df %>% mutate(resid = y - y_hat), aes(ds, resid)) +
    geom_line(color = plot_colors[["Actual"]]) +
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
  model_tag <- if ("model_used" %in% names(df)) unique(df$model_used) else "GLM_NegBin"
  if (length(model_tag) != 1) model_tag <- "GLM_NegBin"
  
  diag_dir <- file.path(metrics_base_dir, tm, "diagnostics")
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  
  ggsave(
    file.path(diag_dir, "forecast_vs_actual.png"),
    plot = plot_forecast_vs_actual(df, tm),
    width = 10, height = 5, dpi = 300
  )
  
  ggsave(
    file.path(diag_dir, "residuals.png"),
    plot = plot_residuals(df, tm),
    width = 10, height = 5, dpi = 300
  )
  
  # Metrics
  m <- compute_metrics(df) %>%
    mutate(team = tm, model = model_tag)
  
  readr::write_csv(
    m,
    file.path(diag_dir, "metrics_final_glm.csv")
  )
  
  metrics_all[[tm]] <- m
}

# ------------------------------------------------------------
# Save metrics (CSV = BA / rapport, RDS = teknisk)
# ------------------------------------------------------------
metrics_final <- bind_rows(metrics_all)

# RDS til intern brug
saveRDS(
  metrics_final,
  file.path(metrics_results_dir, "metrics_final_glm.rds")
)

readr::write_csv(
  metrics_final,
  file.path(metrics_base_dir, "metrics_final_glm.csv")
)

# ------------------------------------------------------------
# Model summary (coefficients) til CSV
# ------------------------------------------------------------
models <- readRDS(here("models", "final_glm_negbin_by_team.rds"))
model_summary <- imap_dfr(models, ~ broom::tidy(.x) %>% mutate(team = .y))

slugify <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

readr::write_csv(
  model_summary,
  file.path(metrics_results_dir, "model_summary_final_glm.csv")
)

walk(unique(model_summary$team), function(tm) {
  out_path <- file.path(
    metrics_base_dir,
    tm,
    "diagnostics",
    "model_summary_final_glm.csv"
  )
  readr::write_csv(filter(model_summary, team == tm), out_path)
  
  readr::write_csv(
    filter(model_summary, team == tm),
    file.path(metrics_results_dir, paste0("model_summary_final_glm_", slugify(tm), ".csv"))
  )
})

# Model print
#NO
mod_no <- readRDS("models/final_glm_negbin_by_team.rds")[[ "Team NO 1, Travelcare" ]]
summary(mod_no)

#DK
mod_dk <- readRDS("models/final_glm_negbin_by_team.rds")[[ "Team DK 1, Travelcare" ]]
summary(mod_dk)

#FI
mod_fi <- readRDS("models/final_glm_negbin_by_team.rds")[[ "Team FI 1, Travelcare" ]]
summary(mod_fi)

#SE 1
mod_se1 <- readRDS("models/final_glm_negbin_by_team.rds")[[ "Team SE 1, Travelcare" ]]
summary(mod_se1)

#SE 2
mod_se2 <- readRDS("models/final_glm_negbin_by_team.rds")[[ "Team SE 2, Travelcare" ]]
summary(mod_se2)

message("✔ Plotting complete.")
message("✔ Metrics saved to: ")
message("  - RDS: ", file.path(metrics_results_dir, "metrics_final_glm.rds"))
print(metrics_final)
