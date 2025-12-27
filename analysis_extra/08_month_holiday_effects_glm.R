# ============================================================
# 08_month_holiday_effects_glm.R
# ------------------------------------------------------------
# Formål:
# - Visualisere GLM-koefficienter (exp(beta)) for month + ferie
# - BA-venlig fremstilling af multiplikativ effekt pr. team
#
# Output:
# - <output_base>/analysis_extra/month_holiday_effects/month_effects_by_team.png
# - <output_base>/analysis_extra/month_holiday_effects/holiday_effects_by_team.png
# - <output_base>/analysis_extra/month_holiday_effects/month_holiday_heatmap.png
# - <output_base>/analysis_extra/month_holiday_effects/month_holiday_effects.csv
# ============================================================

library(tidyverse)
library(here)
library(scales)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "month_holiday_effects")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

models <- readRDS(here("models", "final_glm_negbin_by_team.rds"))

focus_teams <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare",
  "Team SE 2, Travelcare"
)

team_colors <- c(
  "Team DK 1, Travelcare" = "#D93945",
  "Team SE 1, Travelcare" = "#3E3E3E",
  "Team SE 2, Travelcare" = "#2E9A5D",
  "Team NO 1, Travelcare" = "#2A6F97",
  "Team FI 1, Travelcare" = "#8E5C9E"
)

get_coef_df <- function(mod, team) {
  coefs <- broom::tidy(mod) %>%
    select(term, estimate) %>%
    mutate(team = team)
  
  # Month terms: month2..month12 (baseline = month1)
  month_terms <- coefs %>%
    filter(str_detect(term, "^month")) %>%
    mutate(
      feature = "month",
      level = str_replace(term, "^month", "")
    )
  
  # Add baseline month1 with effect = 1
  month_base <- tibble(
    term = "month1",
    estimate = 0,
    team = team,
    feature = "month",
    level = "1"
  )
  
  month_df <- bind_rows(month_terms, month_base) %>%
    mutate(
      effect = exp(estimate),
      level = as.character(level)
    )
  
  holiday_terms <- c("Juleferie", "Vinterferie", "Påskeferie",
                     "Sommerferie", "Efterårsferie")
  
  holiday_df <- coefs %>%
    filter(term %in% holiday_terms) %>%
    mutate(
      feature = "holiday",
      level = term,
      effect = exp(estimate)
    )
  
  bind_rows(month_df, holiday_df)
}

effects_df <- map_dfr(
  intersect(names(models), focus_teams),
  function(tm) get_coef_df(models[[tm]], tm)
)

write_csv(effects_df, file.path(out_dir, "month_holiday_effects.csv"))

# --- Month effects (line per team) ---
month_df <- effects_df %>%
  filter(feature == "month") %>%
  mutate(level = factor(level, levels = 1:12))

p_month <- ggplot(month_df, aes(level, effect, group = team, colour = team)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#3E3E3E") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = team_colors) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Månedseffekter (exp(β)) pr. team",
    subtitle = "Baseline (måned 1) = 1.00",
    x = "Måned",
    y = "Multiplikativ effekt",
    colour = "Team"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "month_effects_by_team.png"), p_month,
       width = 10, height = 5.5, dpi = 300)

# --- Holiday effects (bars, facetted) ---
holiday_df <- effects_df %>%
  filter(feature == "holiday") %>%
  mutate(level = factor(level, levels = c(
    "Juleferie", "Vinterferie", "Påskeferie",
    "Sommerferie", "Efterårsferie"
  )))

p_holiday <- ggplot(holiday_df, aes(level, effect, fill = team)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#3E3E3E") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = team_colors) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Ferieeffekter (exp(β)) pr. team",
    subtitle = "Effekt > 1 = højere volumen, < 1 = lavere volumen",
    x = NULL,
    y = "Multiplikativ effekt",
    fill = "Team"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "holiday_effects_by_team.png"), p_holiday,
       width = 11, height = 5.5, dpi = 300)

# --- Heatmap (team x month/holiday) ---
heat_df <- effects_df %>%
  mutate(
    feature_label = if_else(
      feature == "month",
      paste0("M", level),
      as.character(level)
    )
  )

p_heat <- ggplot(heat_df, aes(feature_label, team, fill = effect)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#3E3E3E", mid = "white", high = "#D93945",
    midpoint = 1, labels = number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Måned + ferieeffekter (exp(β)) – heatmap",
    x = "Feature",
    y = "Team",
    fill = "Effect"
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank())

ggsave(file.path(out_dir, "month_holiday_heatmap.png"), p_heat,
       width = 12, height = 4.2, dpi = 300)

message("✔ CSV: ", file.path(out_dir, "month_holiday_effects.csv"))
message("✔ Plots saved to: ", out_dir)
