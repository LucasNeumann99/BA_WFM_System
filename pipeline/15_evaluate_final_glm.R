# ============================================================
# 15_evaluate_final_glm.R
# ------------------------------------------------------------
# Formål: evaluere endelig GLM pr. team (lag eller baseline), gemme plots og metrics til rapport/teknik.
# - Produces 2 plots per team:
#     1) Forecast vs actual
#     2) Residuals over time
# - Saves metrics as:
#     - CSV (til BA/rapport): output/diagnostics/metrics_final_glm.csv
#     - RDS (til modeller/teknik): results/final/glm/metrics_final_glm.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(broom)

# ------------------------------------------------------------
# Load forecasts
# ------------------------------------------------------------
fc_path <- here("results", "final", "glm", "fc_final_glm_negbin.rds")
fc <- readRDS(fc_path)

teams <- unique(fc$team)

# ------------------------------------------------------------
# Directory setup
# ------------------------------------------------------------
metrics_csv_dir  <- here("output", "diagnostics")   # til BA / rapport
metrics_rds_dir  <- here("results", "final", "glm") # teknisk lagring
fig_dir          <- here("figures", "final", "glm")

dir.create(metrics_csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(metrics_rds_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir,         recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Metric functions (24h – ingen 07–22 filter)
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
  model_tag <- if ("model_used" %in% names(df)) unique(df$model_used) else "GLM_NegBin"
  if (length(model_tag) != 1) model_tag <- "GLM_NegBin"
  
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
    mutate(team = tm, model = model_tag)
  
  metrics_all[[tm]] <- m
}

# ------------------------------------------------------------
# Save metrics (CSV = BA / rapport, RDS = teknisk)
# ------------------------------------------------------------
metrics_final <- bind_rows(metrics_all)

# CSV til rapport
readr::write_csv(
  metrics_final,
  file.path(metrics_csv_dir, "metrics_final_glm.csv")
)

# RDS til intern brug
saveRDS(
  metrics_final,
  file.path(metrics_rds_dir, "metrics_final_glm.rds")
)

# ------------------------------------------------------------
# Model summary (coefficients) til CSV
# ------------------------------------------------------------
models <- readRDS(here("models", "final_glm_negbin_by_team.rds"))
model_summary <- imap_dfr(models, ~ broom::tidy(.x) %>% mutate(team = .y))

# Gem samlet fil (beholder kompatibilitet) og én fil pr. team for nem deling
readr::write_csv(
  model_summary,
  file.path(metrics_csv_dir, "model_summary_final_glm.csv")
)

slugify <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

walk(unique(model_summary$team), function(tm) {
  out_path <- file.path(
    metrics_csv_dir,
    paste0("model_summary_final_glm_", slugify(tm), ".csv")
  )
  readr::write_csv(filter(model_summary, team == tm), out_path)
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
message("  - CSV: output/diagnostics/metrics_final_glm.csv")
message("  - RDS: results/final/glm/metrics_final_glm.rds")
print(metrics_final)
