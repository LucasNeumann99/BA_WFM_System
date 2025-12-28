# ============================================================
# 11_se_routing_before_after.R
# ------------------------------------------------------------
# Formål:
# - Sammenlign "før" vs "efter" for Team SE 1 og SE 2
#   * Koefficienter (month + holidays)
#   * Nøgletal (RMSE, Bias_mean)
# Output:
# - <output_base>/analysis_extra/Strukturelle ændringer/Team_se_routing_problem/
#   - se_before_after_coefficients.csv
#   - se_before_after_metrics.csv
# ============================================================

library(tidyverse)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(
  paths$output,
  "analysis_extra",
  "Strukturelle ændringer",
  "Team_se_routing_problem"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

teams <- c("Team SE 1, Travelcare", "Team SE 2, Travelcare")

# ------------------------------------------------------------
# Inputs
# ------------------------------------------------------------
old_dir <- file.path(out_dir, "old_diagnostics_%s")
new_diag_dir <- file.path(paths$output, "diagnostics", "%s")

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    stop("Mangler fil: ", path)
  }
  readr::read_csv(path, show_col_types = FALSE)
}

coeff_terms <- c(
  paste0("month", 2:12),
  "Juleferie", "Vinterferie", "Påskeferie", "Sommerferie", "Efterårsferie"
)

coeff_before <- map_dfr(teams, function(tm) {
  path <- sprintf(old_dir, tm)
  read_csv_safe(file.path(path, "model_summary_final_glm.csv")) %>%
    filter(term %in% coeff_terms) %>%
    mutate(team = tm, version = "before")
})

coeff_after <- map_dfr(teams, function(tm) {
  path <- sprintf(new_diag_dir, tm)
  read_csv_safe(file.path(path, "model_summary_operational.csv")) %>%
    filter(term %in% coeff_terms) %>%
    mutate(team = tm, version = "after")
})

coeff_comp <- bind_rows(coeff_before, coeff_after) %>%
  select(team, version, term, estimate) %>%
  pivot_wider(
    names_from = version,
    values_from = estimate
  ) %>%
  mutate(diff = after - before) %>%
  arrange(team, term)

readr::write_csv(
  coeff_comp,
  file.path(out_dir, "se_before_after_coefficients.csv")
)

# ------------------------------------------------------------
# Metrics: RMSE + Bias_mean
# ------------------------------------------------------------
metrics_before <- map_dfr(teams, function(tm) {
  path <- sprintf(old_dir, tm)
  read_csv_safe(file.path(path, "metrics_final_glm.csv")) %>%
    mutate(team = tm, version = "before")
})

metrics_after <- map_dfr(teams, function(tm) {
  path <- sprintf(new_diag_dir, tm)
  read_csv_safe(file.path(path, "metrics_operational.csv")) %>%
    mutate(team = tm, version = "after")
})

metrics_comp <- bind_rows(metrics_before, metrics_after) %>%
  select(team, version, RMSE, Bias_mean) %>%
  pivot_wider(
    names_from = version,
    values_from = c(RMSE, Bias_mean)
  ) %>%
  mutate(
    RMSE_diff = RMSE_after - RMSE_before,
    Bias_mean_diff = Bias_mean_after - Bias_mean_before
  ) %>%
  arrange(team)

readr::write_csv(
  metrics_comp,
  file.path(out_dir, "se_before_after_metrics.csv")
)

message("✔ SE before/after outputs written to: ", out_dir)
