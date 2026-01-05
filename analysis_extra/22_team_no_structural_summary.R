#!/usr/bin/env Rscript
# ============================================================
# 22_team_no_structural_summary.R
# ------------------------------------------------------------
# Summaries + plots to support the Team NO structural adjustment story.
# Outputs to: BA_WFM_Output/analysis_extra/Strukturelle ændringer/Team_no_customer_exit
# ============================================================

library(tidyverse)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "Strukturelle ændringer", "Team_no_customer_exit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

comparison_path <- file.path(out_dir, "team_no_adjustment_comparison.csv")
delta_path <- file.path(out_dir, "team_no_adjustment_delta.csv")
level_path <- file.path(out_dir, "team_no_level_factor.csv")

comparison <- readr::read_csv(comparison_path, show_col_types = FALSE)
delta <- readr::read_csv(delta_path, show_col_types = FALSE)
level_factor <- readr::read_csv(level_path, show_col_types = FALSE)

summary_out <- comparison %>%
  select(variant, RMSE, MAE, MAPE, SMAPE, Bias_mean, Bias_sd) %>%
  mutate(level_factor = level_factor$level_factor[1])

readr::write_csv(
  summary_out,
  file.path(out_dir, "team_no_adjustment_summary.csv")
)

# ------------------------------------------------------------
# Plot: RMSE + Bias before/after
# ------------------------------------------------------------
plot_df <- comparison %>%
  select(variant, RMSE, Bias_mean) %>%
  pivot_longer(cols = c(RMSE, Bias_mean), names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(metric, RMSE = "RMSE", Bias_mean = "Bias (mean)"),
    variant = factor(variant, levels = c("raw", "adjusted"))
  )

p_metrics <- ggplot(plot_df, aes(variant, value, fill = variant)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(value, 2)), vjust = -0.4, size = 3.5) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(values = c("raw" = "#3E3E3E", "adjusted" = "#D93945")) +
  labs(
    title = "Team NO - bias og RMSE før/efter justering",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "team_no_bias_rmse_before_after.png"),
  p_metrics,
  width = 7.5,
  height = 4.5,
  dpi = 300
)

# ------------------------------------------------------------
# Plot: Level factor (scaling)
# ------------------------------------------------------------
lf_value <- level_factor$level_factor[1]
lf_df <- tibble(label = "Level factor", value = lf_value)

p_lf <- ggplot(lf_df, aes(label, value)) +
  geom_col(fill = "#2A6F97", width = 0.4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#3E3E3E") +
  geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 3.5) +
  ylim(0, max(1.05, lf_value + 0.1)) +
  labs(
    title = "Team NO - niveaujustering før 2025-06-01",
    x = "",
    y = "Skaleringsfaktor"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_blank())

ggsave(
  file.path(out_dir, "team_no_level_factor.png"),
  p_lf,
  width = 6,
  height = 4,
  dpi = 300
)

message("Team NO structural summary saved to: ", out_dir)
