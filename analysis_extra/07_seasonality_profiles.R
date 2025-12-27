# ============================================================
# 07_seasonality_profiles.R
# ------------------------------------------------------------
# Formål:
# - Visualisere "sæsonprofiler" fra de endelige GLM-modeller
#   (month, weekday, hour) pr. team
# - Normaliseret indeks (mu / mean(mu)) for sammenlignelighed
#
# Output:
# - <output_base>/analysis_extra/seasonality_profiles/*.png
# - <output_base>/analysis_extra/seasonality_profiles/*.csv
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(patchwork)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "seasonality_profiles")
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

build_profile <- function(mod, team, feature) {
  xlv <- mod$xlevels
  hour_levels <- xlv$hour %||% as.character(0:23)
  weekday_levels <- xlv$weekday %||% c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  month_levels <- xlv$month %||% as.character(1:12)
  
  base <- tibble(
    hour = factor(hour_levels[1], levels = hour_levels),
    weekday = factor(weekday_levels[1], levels = weekday_levels),
    month = factor(month_levels[1], levels = month_levels),
    year_c = 0,
    year = 2024,
    week = 1,
    Juleferie = 0, Vinterferie = 0, Påskeferie = 0,
    Sommerferie = 0, Efterårsferie = 0
  )
  
  if (feature == "month") {
    df <- base[rep(1, length(month_levels)), ]
    df$month <- factor(month_levels, levels = month_levels)
    df$index <- month_levels
  } else if (feature == "weekday") {
    df <- base[rep(1, length(weekday_levels)), ]
    df$weekday <- factor(weekday_levels, levels = weekday_levels)
    df$index <- weekday_levels
  } else if (feature == "hour") {
    df <- base[rep(1, length(hour_levels)), ]
    df$hour <- factor(hour_levels, levels = hour_levels)
    df$index <- hour_levels
  } else {
    stop("Unknown feature: ", feature)
  }
  
  mu <- predict(mod, newdata = df, type = "response")
  df %>%
    mutate(
      team = team,
      feature = feature,
      mu = mu,
      index_value = mu / mean(mu, na.rm = TRUE)
    ) %>%
    select(team, feature, index, index_value)
}

profiles <- map_dfr(
  intersect(names(models), focus_teams),
  function(tm) {
    mod <- models[[tm]]
    bind_rows(
      build_profile(mod, tm, "month"),
      build_profile(mod, tm, "weekday"),
      build_profile(mod, tm, "hour")
    )
  }
)

write_csv(profiles, file.path(out_dir, "seasonality_profiles.csv"))

month_df <- profiles %>%
  filter(feature == "month") %>%
  mutate(
    month = as.integer(index),
    month = factor(month, levels = 1:12)
  )

p_month <- ggplot(month_df, aes(month, index_value, group = team, colour = team)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = team_colors) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Sæsonprofil: månedseffekt (normaliseret)",
    x = "Måned",
    y = "Indeks (mu / mean)",
    colour = "Team"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "seasonality_month_by_team.png"), p_month,
       width = 10, height = 5.5, dpi = 300)

weekday_df <- profiles %>%
  filter(feature == "weekday") %>%
  mutate(
    weekday = factor(index, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

p_weekday <- ggplot(weekday_df, aes(weekday, index_value, group = team, colour = team)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = team_colors) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Sæsonprofil: ugedagseffekt (normaliseret)",
    x = "Ugedag",
    y = "Indeks (mu / mean)",
    colour = "Team"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "seasonality_weekday_by_team.png"), p_weekday,
       width = 10, height = 5.5, dpi = 300)

hour_df <- profiles %>%
  filter(feature == "hour") %>%
  mutate(hour = as.integer(index))

p_hour <- ggplot(hour_df, aes(hour, index_value, group = team, colour = team)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = team_colors) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Sæsonprofil: timeeffekt (normaliseret)",
    x = "Time på dagen",
    y = "Indeks (mu / mean)",
    colour = "Team"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "seasonality_hour_by_team.png"), p_hour,
       width = 10, height = 5.5, dpi = 300)

# ---------------- Additional variants ----------------

# Facets by team (month/weekday/hour)
facet_base <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())

p_month_facet <- ggplot(month_df, aes(month, index_value, group = 1)) +
  geom_line(color = "#D93945", linewidth = 0.9) +
  geom_point(color = "#D93945", size = 1.4) +
  facet_wrap(~ team, scales = "free_y") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Månedseffekt pr. team (normaliseret)",
    x = "Måned",
    y = "Indeks (mu / mean)"
  ) +
  facet_base

ggsave(file.path(out_dir, "seasonality_month_facets.png"), p_month_facet,
       width = 10, height = 7, dpi = 300)

p_weekday_facet <- ggplot(weekday_df, aes(weekday, index_value, group = 1)) +
  geom_line(color = "#2A6F97", linewidth = 0.9) +
  geom_point(color = "#2A6F97", size = 1.4) +
  facet_wrap(~ team, scales = "free_y") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Ugedagseffekt pr. team (normaliseret)",
    x = "Ugedag",
    y = "Indeks (mu / mean)"
  ) +
  facet_base

ggsave(file.path(out_dir, "seasonality_weekday_facets.png"), p_weekday_facet,
       width = 10, height = 7, dpi = 300)

p_hour_facet <- ggplot(hour_df, aes(hour, index_value, group = 1)) +
  geom_line(color = "#2E9A5D", linewidth = 0.9) +
  geom_point(color = "#2E9A5D", size = 1.4) +
  facet_wrap(~ team, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Timeeffekt pr. team (normaliseret)",
    x = "Time på dagen",
    y = "Indeks (mu / mean)"
  ) +
  facet_base

ggsave(file.path(out_dir, "seasonality_hour_facets.png"), p_hour_facet,
       width = 10, height = 7, dpi = 300)

# Heatmaps (team x month/weekday/hour)
heat_base <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())

p_month_hm <- ggplot(month_df, aes(month, team, fill = index_value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#3E3E3E", mid = "white", high = "#D93945", midpoint = 1) +
  labs(
    title = "Heatmap: månedseffekt pr. team (normaliseret)",
    x = "Måned",
    y = "Team",
    fill = "Indeks"
  ) +
  heat_base

ggsave(file.path(out_dir, "seasonality_month_heatmap.png"), p_month_hm,
       width = 9.5, height = 3.8, dpi = 300)

p_weekday_hm <- ggplot(weekday_df, aes(weekday, team, fill = index_value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#3E3E3E", mid = "white", high = "#2A6F97", midpoint = 1) +
  labs(
    title = "Heatmap: ugedagseffekt pr. team (normaliseret)",
    x = "Ugedag",
    y = "Team",
    fill = "Indeks"
  ) +
  heat_base

ggsave(file.path(out_dir, "seasonality_weekday_heatmap.png"), p_weekday_hm,
       width = 9.5, height = 3.8, dpi = 300)

p_hour_hm <- ggplot(hour_df, aes(factor(hour), team, fill = index_value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#3E3E3E", mid = "white", high = "#2E9A5D", midpoint = 1) +
  labs(
    title = "Heatmap: timeeffekt pr. team (normaliseret)",
    x = "Time",
    y = "Team",
    fill = "Indeks"
  ) +
  heat_base

ggsave(file.path(out_dir, "seasonality_hour_heatmap.png"), p_hour_hm,
       width = 12, height = 3.8, dpi = 300)

# One-pager (3 panels: month/weekday/hour by team)
month_small <- p_month +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  )
weekday_small <- p_weekday +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  )
hour_small <- p_hour +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  )

one_pager <- (month_small / weekday_small / hour_small) +
  patchwork::plot_annotation(
    title = "Sæsonprofiler pr. team (normaliseret indeks)",
    subtitle = "Måned, ugedag og time – sammenlignede profiler på tværs af teams"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11)
  )

ggsave(file.path(out_dir, "seasonality_one_pager.png"), one_pager,
       width = 10, height = 14, dpi = 300)

message("✔ Profiles CSV: ", file.path(out_dir, "seasonality_profiles.csv"))
message("✔ Plots saved to: ", out_dir)
