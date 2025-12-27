# ============================================================
# 06_call_funnel_real_offered_lost.R
# ------------------------------------------------------------
# Formål:
# - Vis tragt fra total calls → offered → real offered → answered
# - Kvantificér lost calls (ikke når alarmcentralen) pr. team
#
# Output:
# - <output_base>/analysis_extra/call_funnel_by_team.png
# - <output_base>/analysis_extra/call_lost_share_by_team.png
# - <output_base>/analysis_extra/call_funnel_summary.csv
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(scales)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

calls <- readRDS(here("data_processed", "calls_clean.rds"))

focus_teams <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare",
  "Team SE 2, Travelcare"
)

# Helper: robust 0/1 conversion (TRUE/FALSE/1/0/NA)
as_01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) return(as.integer(x > 0))
  if (is.character(x)) return(as.integer(tolower(x) %in% c("true", "t", "1", "yes", "y")))
  as.integer(x)
}

plot_colors <- c(
  "Total Calls"      = "#3E3E3E", # Gray_dark
  "Offered Calls"    = "#2A6F97", # Blue
  "Real Offered"     = "#D93945", # SOS_red
  "Answered Calls"   = "#2E9A5D"  # Green
)

df <- calls %>%
  filter(call_responsible_department_l5 %in% focus_teams) %>%
  mutate(
    offered_call     = as_01(offered_call),
    real_offered_call= as_01(real_offered_call),
    answered_call    = as_01(answered_call),
    lost_call        = as_01(lost_call),
    real_lost_call   = as_01(real_lost_call)
  )

team_summary <- df %>%
  group_by(call_responsible_department_l5) %>%
  summarise(
    total_calls       = n(),
    offered_calls     = sum(offered_call, na.rm = TRUE),
    real_offered_calls= sum(real_offered_call, na.rm = TRUE),
    answered_calls    = sum(answered_call, na.rm = TRUE),
    lost_calls        = sum(real_lost_call, na.rm = TRUE),
    lost_share        = if_else(real_offered_calls > 0,
                                lost_calls / real_offered_calls,
                                NA_real_),
    .groups = "drop"
  ) %>%
  rename(team = call_responsible_department_l5)

overall_summary <- team_summary %>%
  summarise(
    team = "All Teams",
    total_calls = sum(total_calls),
    offered_calls = sum(offered_calls),
    real_offered_calls = sum(real_offered_calls),
    answered_calls = sum(answered_calls),
    lost_calls = sum(lost_calls),
    lost_share = if_else(sum(real_offered_calls) > 0,
                         sum(lost_calls) / sum(real_offered_calls),
                         NA_real_)
  )

summary_out <- bind_rows(team_summary, overall_summary)

readr::write_csv(summary_out, file.path(out_dir, "call_funnel_summary.csv"))

funnel_df <- summary_out %>%
  filter(team != "All Teams") %>%
  pivot_longer(
    cols = c(total_calls, offered_calls, real_offered_calls, answered_calls),
    names_to = "stage",
    values_to = "count"
  ) %>%
  mutate(
    stage = recode(stage,
                   total_calls = "Total Calls",
                   offered_calls = "Offered Calls",
                   real_offered_calls = "Real Offered",
                   answered_calls = "Answered Calls"),
    stage = factor(stage, levels = c("Total Calls", "Offered Calls", "Real Offered", "Answered Calls"))
  )

p <- ggplot(funnel_df, aes(x = stage, y = count, fill = stage)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(count)), hjust = -0.1, size = 3.2) +
  coord_flip() +
  facet_wrap(~ team, scales = "free_x") +
  scale_fill_manual(values = plot_colors) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Call funnel pr. team: Total → Offered → Real Offered → Answered",
    subtitle = "Lost calls (real_lost_call) beregnes separat pr. team",
    x = NULL,
    y = "Antal opkald",
    fill = "Stage"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = file.path(out_dir, "call_funnel_by_team.png"),
  plot = p,
  width = 12, height = 8, dpi = 300
)

lost_plot_df <- summary_out %>%
  filter(team != "All Teams") %>%
  arrange(desc(lost_share))

p_lost <- ggplot(lost_plot_df, aes(x = team, y = lost_share, fill = team)) +
  geom_col() +
  geom_text(
    aes(label = percent(lost_share, accuracy = 0.1)),
    vjust = -0.4,
    size = 3.4
  ) +
  scale_fill_manual(values = c(
    "Team DK 1, Travelcare" = "#D93945",
    "Team SE 1, Travelcare" = "#3E3E3E",
    "Team SE 2, Travelcare" = "#2E9A5D",
    "Team NO 1, Travelcare" = "#2A6F97",
    "Team FI 1, Travelcare" = "#8E5C9E"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Andel lost calls pr. team",
    subtitle = "lost_share = real_lost_call / real_offered_call",
    x = "Team",
    y = "Lost share"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = file.path(out_dir, "call_lost_share_by_team.png"),
  plot = p_lost,
  width = 10, height = 5.5, dpi = 300
)

message("✔ Funnel CSV: ", file.path(out_dir, "call_funnel_summary.csv"))
message("✔ Funnel plot: ", file.path(out_dir, "call_funnel_by_team.png"))
message("✔ Lost-share plot: ", file.path(out_dir, "call_lost_share_by_team.png"))
