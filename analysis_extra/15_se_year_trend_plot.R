# ============================================================
# 15_se_year_trend_plot.R
# ------------------------------------------------------------
# Formål:
# - Visualisere årlig vækst fra year_c-koefficienter (log-skala -> pct)
# - Sammenligne gamle SE1/SE2 modeller vs. ny SE_total
# Output:
# - <output_base>/analysis_extra/Strukturelle ændringer/Team_se_routing_problem/
#   se_year_trend_comparison.csv
#   se_year_trend_comparison.png
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

# ------------------------------------------------------------
# Old models: final GLM (SE1/SE2)
# ------------------------------------------------------------
old_models <- readRDS(here("models", "final_glm_negbin_by_team.rds"))

get_year_coef <- function(mod) {
  coef_val <- coef(mod)
  if (!"year_c" %in% names(coef_val)) {
    return(NA_real_)
  }
  as.numeric(coef_val[["year_c"]])
}

se1_old <- get_year_coef(old_models[["Team SE 1, Travelcare"]])
se2_old <- get_year_coef(old_models[["Team SE 2, Travelcare"]])

# ------------------------------------------------------------
# New model: SE total summary (operative)
# ------------------------------------------------------------
se_total_summary_path <- file.path(
  paths$output,
  "diagnostics",
  "Team SE total",
  "model_summary_operational.csv"
)
if (!file.exists(se_total_summary_path)) {
  stop("Mangler SE total model summary: ", se_total_summary_path)
}

se_total_summary <- readr::read_csv(se_total_summary_path, show_col_types = FALSE)
se_total_year <- se_total_summary %>%
  filter(term == "year_c") %>%
  pull(estimate)
se_total_year <- if (length(se_total_year) == 0) NA_real_ else as.numeric(se_total_year[1])

trend_df <- tibble(
  model = c("SE1 (old)", "SE2 (old)", "SE total (new)"),
  year_coef = c(se1_old, se2_old, se_total_year)
) %>%
  mutate(
    annual_growth = exp(year_coef) - 1,
    annual_growth_pct = 100 * annual_growth
  )

readr::write_csv(
  trend_df,
  file.path(out_dir, "se_year_trend_comparison.csv")
)

plot_df <- trend_df %>%
  mutate(
    label = sprintf("%.1f%%", annual_growth_pct),
    model = factor(model, levels = model)
  )

plot_colors <- c(
  "SE1 (old)" = "#D93945",
  "SE2 (old)" = "#F46A74",
  "SE total (new)" = "#3E3E3E"
)

p <- ggplot(plot_df, aes(model, annual_growth_pct, fill = model)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label), vjust = -0.4, size = 4) +
  scale_fill_manual(values = plot_colors) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Årlig trend fra year-koefficienter (log → pct)",
    subtitle = "Ny SE_total giver fælles ~8% trend, split arver samme vækst",
    x = "",
    y = "Årlig vækst (%)",
    fill = ""
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

out_plot <- file.path(out_dir, "se_year_trend_comparison.png")
ggsave(out_plot, plot = p, width = 8, height = 5, dpi = 300)

message("✔ CSV: ", file.path(out_dir, "se_year_trend_comparison.csv"))
message("✔ Plot: ", out_plot)
