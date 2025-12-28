# ============================================================
# 14_se_actual_share_last6m.R
# ------------------------------------------------------------
# Formål:
# - Faktisk share mellem SE1/SE2 baseret på Real Offered Calls
# - Seneste 6 måneder + YoY sammenligning
# Output:
# - <output_base>/analysis_extra/Strukturelle ændringer/Team_se_routing_problem/
#   se_actual_share_last6m.csv
#   se_actual_share_last6m_summary.csv
#   se_actual_share_last6m.png
# ============================================================

library(tidyverse)
library(lubridate)
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

calls <- readRDS(here("data_processed", "calls_clean.rds"))

real_offered_col <- if ("real_offered_call" %in% names(calls)) {
  "real_offered_call"
} else if ("Real_Offered_Call" %in% names(calls)) {
  "Real_Offered_Call"
} else {
  stop("Kunne ikke finde real_offered kolonne i calls_clean.rds")
}

teams <- c("Team SE 1, Travelcare", "Team SE 2, Travelcare")

calls_se <- calls %>%
  mutate(
    call_date = as.Date(call_start_time),
    month = as.Date(floor_date(call_date, "month")),
    real_offered = .data[[real_offered_col]]
  ) %>%
  filter(
    call_responsible_department_l5 %in% teams,
    real_offered %in% c(TRUE, 1)
  )

max_month <- max(calls_se$month, na.rm = TRUE)
last6_months <- seq(max_month %m-% months(5), max_month, by = "1 month")

monthly <- calls_se %>%
  filter(month %in% last6_months) %>%
  count(team = call_responsible_department_l5, month, name = "calls")

monthly <- monthly %>%
  group_by(month) %>%
  mutate(
    total_calls = sum(calls),
    share = if_else(total_calls > 0, calls / total_calls, NA_real_)
  ) %>%
  ungroup()

readr::write_csv(
  monthly,
  file.path(out_dir, "se_actual_share_last6m.csv")
)

# YoY: samme måneder året før
yoy_months <- last6_months %m-% years(1)
yoy <- calls_se %>%
  filter(month %in% yoy_months) %>%
  count(team = call_responsible_department_l5, month, name = "calls") %>%
  group_by(month) %>%
  mutate(
    total_calls = sum(calls),
    share = if_else(total_calls > 0, calls / total_calls, NA_real_)
  ) %>%
  ungroup()

summary_df <- bind_rows(
  monthly %>%
    group_by(team) %>%
    summarise(
      period = "last_6m",
      avg_share = mean(share, na.rm = TRUE),
      avg_calls = mean(calls, na.rm = TRUE),
      .groups = "drop"
    ),
  yoy %>%
    group_by(team) %>%
    summarise(
      period = "last_6m_yoy",
      avg_share = mean(share, na.rm = TRUE),
      avg_calls = mean(calls, na.rm = TRUE),
      .groups = "drop"
    )
) %>%
  arrange(team, period)

readr::write_csv(
  summary_df,
  file.path(out_dir, "se_actual_share_last6m_summary.csv")
)

team_colors <- c(
  "Team SE 1, Travelcare" = "#D93945",
  "Team SE 2, Travelcare" = "#F46A74"
)

p <- ggplot(monthly, aes(month, share, colour = team, group = team)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = team_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme_minimal(base_size = 12) +
  labs(
    title = "SE1 vs SE2 – faktisk share (Real Offered), seneste 6 måneder",
    subtitle = paste0("Seneste måned: ", format(max_month, "%Y-%m")),
    x = "Måned",
    y = "Share",
    colour = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

out_plot <- file.path(out_dir, "se_actual_share_last6m.png")
ggsave(out_plot, plot = p, width = 10, height = 5, dpi = 300)

message("✔ CSV: ", file.path(out_dir, "se_actual_share_last6m.csv"))
message("✔ Summary: ", file.path(out_dir, "se_actual_share_last6m_summary.csv"))
message("✔ Plot: ", out_plot)
