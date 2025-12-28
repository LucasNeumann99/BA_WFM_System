# ============================================================
# 13_real_offered_index_nov_to_nov.R
# ------------------------------------------------------------
# Formål:
# - Rolling 12-måneders index pr. team (hovedgraf)
# - Okt->Okt tabel som supplement
# - Periode: 2022-01-01 til 2025-11-11 (data-drevet)
# Output:
# - <output_base>/analysis_extra/real_offered_index_nov_to_nov.csv
# - <output_base>/analysis_extra/real_offered_index_nov_to_nov.png
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "Y_præsentation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

calls <- readRDS(here("data_processed", "calls_clean.rds"))

team_col <- "call_responsible_department_l5"
start_date <- as.Date("2022-01-01")
end_date <- max(as.Date(calls$call_start_time), na.rm = TRUE)

real_offered_col <- if ("real_offered_call" %in% names(calls)) {
  "real_offered_call"
} else if ("Real_Offered_Call" %in% names(calls)) {
  "Real_Offered_Call"
} else {
  stop("Kunne ikke finde real_offered kolonne i calls_clean.rds")
}

calls <- calls %>%
  mutate(
    call_date = as.Date(call_start_time),
    real_offered = .data[[real_offered_col]]
  ) %>%
  filter(
    !is.na(.data[[team_col]]),
    !is.na(call_date),
    call_date >= start_date,
    call_date <= end_date,
    real_offered %in% c(TRUE, 1)
  )

monthly <- calls %>%
  mutate(month = as.Date(floor_date(call_date, "month"))) %>%
  group_by(team = .data[[team_col]], month) %>%
  summarise(calls = n(), .groups = "drop")

se_total <- monthly %>%
  filter(team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare")) %>%
  group_by(month) %>%
  summarise(calls = sum(calls), .groups = "drop") %>%
  mutate(team = "Team SE total")

monthly <- bind_rows(monthly, se_total) %>%
  group_by(team) %>%
  complete(
    month = seq(min(month), max(month), by = "1 month"),
    fill = list(calls = 0L)
  ) %>%
  arrange(team, month) %>%
  ungroup()

rolling <- monthly %>%
  group_by(team) %>%
  arrange(month) %>%
  mutate(
    roll_12 = as.numeric(stats::filter(calls, rep(1, 12), sides = 1))
  ) %>%
  ungroup()

rolling_index <- rolling %>%
  group_by(team) %>%
  mutate(
    base_roll = first(roll_12[!is.na(roll_12)]),
    index = if_else(base_roll > 0, 100 * roll_12 / base_roll, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(index))

out_csv <- file.path(out_dir, "real_offered_index_rolling_12m.csv")
readr::write_csv(rolling_index, out_csv)

oct_annual <- calls %>%
  mutate(
    oct_year = if_else(month(call_date) >= 10, year(call_date), year(call_date) - 1L)
  ) %>%
  group_by(team = .data[[team_col]], oct_year) %>%
  summarise(calls = n(), .groups = "drop")

baseline_year <- 2022L
baseline <- oct_annual %>%
  filter(oct_year == baseline_year) %>%
  select(team, base_calls = calls)

oct_index <- oct_annual %>%
  left_join(baseline, by = "team") %>%
  mutate(
    index = if_else(base_calls > 0, 100 * calls / base_calls, NA_real_)
  ) %>%
  filter(oct_year >= 2022, oct_year <= 2025)

out_oct_csv <- file.path(out_dir, "real_offered_index_oct_to_oct.csv")
readr::write_csv(oct_index, out_oct_csv)

team_colors <- c(
  "Team SE 1, Travelcare" = "#D93945",
  "Team SE 2, Travelcare" = "#F46A74",
  "Team SE total" = "#3E3E3E",
  "Team DK 1, Travelcare" = "#BDBDBD",
  "Team NO 1, Travelcare" = "#2A6F97",
  "Team FI 1, Travelcare" = "#8E5C9E"
)

team_labels <- c(
  "Team DK 1, Travelcare" = "DK",
  "Team FI 1, Travelcare" = "FI",
  "Team NO 1, Travelcare" = "NO",
  "Team SE 1, Travelcare" = "SE1",
  "Team SE 2, Travelcare" = "SE2",
  "Team SE total" = "SE total"
)

label_df <- rolling_index %>%
  group_by(team) %>%
  filter(month == max(month, na.rm = TRUE)) %>%
  mutate(label = team_labels[team]) %>%
  ungroup()

p <- ggplot(rolling_index, aes(month, index, colour = team, group = team)) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_point(size = 2) +
  geom_text(
    data = label_df,
    aes(label = label),
    hjust = -0.2,
    vjust = 0.5,
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = team_colors) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "SE2 eksploderer, SE1 falder – rullende 12-måneders Real Offered index",
    subtitle = paste0("Base = første fulde 12 mdr. Data t.o.m. ",
                      format(end_date, "%Y-%m-%d")),
    x = "Måned",
    y = "Index",
    colour = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

out_plot <- file.path(out_dir, "real_offered_index_rolling_12m.png")
ggsave(out_plot, plot = p, width = 11, height = 5.5, dpi = 300)

message("✔ CSV: ", out_csv)
message("✔ Oct->Oct CSV: ", out_oct_csv)
message("✔ Plot: ", out_plot)
