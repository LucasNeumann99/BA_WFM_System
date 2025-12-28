# ============================================================
# 17_model_story_summary.R
# ------------------------------------------------------------
# Formål:
# - Samle bias, annual trend og month/holiday effekter for 5 teams
# Output:
# - <output_base>/analysis_extra/model_story/
#   - team_bias_overview.csv
#   - team_annual_trend.csv
#   - team_month_holiday_effects.csv
#   - team_annual_trend.png
# ============================================================

library(tidyverse)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "model_story")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

teams <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare",
  "Team SE 2, Travelcare"
)

metrics_paths <- file.path(paths$output, "diagnostics", teams, "metrics_operational.csv")

metrics_df <- purrr::map_dfr(metrics_paths, function(p) {
  if (!file.exists(p)) {
    warning("Mangler metrics: ", p)
    return(tibble())
  }
  readr::read_csv(p, show_col_types = FALSE)
}) %>%
  select(team, RMSE, Bias_mean)

team_colors <- c(
  "Team DK 1, Travelcare" = "#D93945",
  "Team SE 1, Travelcare" = "#3E3E3E",
  "Team SE 2, Travelcare" = "#2E9A5D",
  "Team NO 1, Travelcare" = "#2A6F97",
  "Team FI 1, Travelcare" = "#8E5C9E"
)

readr::write_csv(metrics_df, file.path(out_dir, "team_bias_overview.csv"))

plot_bias <- metrics_df %>%
  mutate(team = factor(team, levels = teams))

bias_plot <- ggplot(plot_bias, aes(team, Bias_mean, fill = team)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Bias_mean)), vjust = -0.4, size = 3.6) +
  scale_fill_manual(values = team_colors) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Gennemsnitlig bias pr. team (operativ model)",
    subtitle = "Positive værdier = systematisk overforecast",
    x = "",
    y = "Bias (opkald/time)",
    fill = ""
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(file.path(out_dir, "team_bias_overview.png"), plot = bias_plot, width = 9, height = 4.8, dpi = 300)

summary_path <- file.path(paths$output, "diagnostics", "Total_Travelcare", "model_summary_operational.csv")
if (!file.exists(summary_path)) {
  stop("Mangler model summary: ", summary_path)
}

summary_df <- readr::read_csv(summary_path, show_col_types = FALSE)

year_terms <- summary_df %>%
  filter(term == "year_c") %>%
  mutate(
    annual_growth = exp(estimate) - 1,
    annual_growth_pct = 100 * annual_growth
  ) %>%
  select(team, estimate, annual_growth_pct) %>%
  rename(year_coef = estimate)

se_total_year <- year_terms %>%
  filter(team == "Team SE total") %>%
  select(year_coef, annual_growth_pct)

if (nrow(se_total_year) == 1) {
  year_terms <- year_terms %>%
    filter(!team %in% c("Team SE total", "Team SE 1, Travelcare", "Team SE 2, Travelcare")) %>%
    bind_rows(
      tibble(
        team = "Team SE 1, Travelcare",
        year_coef = se_total_year$year_coef,
        annual_growth_pct = se_total_year$annual_growth_pct,
        source_model = "Team SE total"
      ),
      tibble(
        team = "Team SE 2, Travelcare",
        year_coef = se_total_year$year_coef,
        annual_growth_pct = se_total_year$annual_growth_pct,
        source_model = "Team SE total"
      )
    )
} else {
  year_terms <- year_terms %>%
    mutate(source_model = team)
}

readr::write_csv(
  year_terms %>% filter(team %in% teams),
  file.path(out_dir, "team_annual_trend.csv")
)

coeff_terms <- c(
  paste0("month", 2:12),
  "Juleferie", "Vinterferie", "Påskeferie", "Sommerferie", "Efterårsferie"
)

effects_df <- summary_df %>%
  filter(term %in% coeff_terms) %>%
  mutate(
    effect_pct = (exp(estimate) - 1) * 100
  ) %>%
  select(team, term, estimate, effect_pct)

se_total_effects <- effects_df %>%
  filter(team == "Team SE total") %>%
  select(term, estimate, effect_pct)

if (nrow(se_total_effects) > 0) {
  effects_df <- effects_df %>%
    filter(!team %in% c("Team SE total", "Team SE 1, Travelcare", "Team SE 2, Travelcare")) %>%
    bind_rows(
      se_total_effects %>% mutate(team = "Team SE 1, Travelcare", source_model = "Team SE total"),
      se_total_effects %>% mutate(team = "Team SE 2, Travelcare", source_model = "Team SE total")
    )
} else {
  effects_df <- effects_df %>% mutate(source_model = team)
}

readr::write_csv(
  effects_df %>% filter(team %in% teams),
  file.path(out_dir, "team_month_holiday_effects.csv")
)

plot_df <- year_terms %>%
  filter(team %in% teams) %>%
  mutate(team = factor(team, levels = teams))

p <- ggplot(plot_df, aes(team, annual_growth_pct, fill = team)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", annual_growth_pct)), vjust = -0.4, size = 3.6) +
  scale_fill_manual(values = team_colors) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Årlig trend (year_c) pr. team",
    subtitle = "SE1/SE2 arver trend fra SE total-model",
    x = "",
    y = "Årlig vækst (%)",
    fill = ""
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(file.path(out_dir, "team_annual_trend.png"), plot = p, width = 9, height = 4.8, dpi = 300)

message("✔ Output skrevet til: ", out_dir)
