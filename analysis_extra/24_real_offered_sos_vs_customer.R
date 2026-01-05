#!/usr/bin/env Rscript
# ============================================================
# 24_real_offered_sos_vs_customer.R
# ------------------------------------------------------------
# Plot Real Offered Calls split: SOS hovedlinje vs kundelinje pr. team
# Output: BA_WFM_Output/analysis_extra/Y_præsentation
# ============================================================

library(tidyverse)
library(here)
library(scales)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(paths$output, "analysis_extra", "Y_præsentation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

calls <- readRDS(here("data_processed", "calls_clean.rds"))

# Normalize column names if needed
if (!"call_responsible_department_l5" %in% names(calls) &&
    "Call_Responsible_Department_L5" %in% names(calls)) {
  calls <- calls %>% rename(call_responsible_department_l5 = Call_Responsible_Department_L5)
}
if (!"customer_profile" %in% names(calls) && "Customer_Profile" %in% names(calls)) {
  calls <- calls %>% rename(customer_profile = Customer_Profile)
}
if (!"real_offered_call" %in% names(calls) && "Real_Offered_Call" %in% names(calls)) {
  calls <- calls %>% rename(real_offered_call = Real_Offered_Call)
}

required_cols <- c("call_responsible_department_l5", "customer_profile", "real_offered_call")
missing_cols <- setdiff(required_cols, names(calls))
if (length(missing_cols) > 0) {
  stop("Missing required columns in calls_clean.rds: ", paste(missing_cols, collapse = ", "))
}

focus_teams <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare",
  "Team SE 2, Travelcare"
)

sos_line <- "SOS International Denmark"

real_offered_by_team <- calls %>%
  filter(real_offered_call == 1) %>%
  filter(call_responsible_department_l5 %in% focus_teams) %>%
  group_by(call_responsible_department_l5) %>%
  summarise(
    total_real_offered = n(),
    .groups = "drop"
  )

calls_sos_by_team <- calls %>%
  filter(real_offered_call == 1) %>%
  filter(
    call_responsible_department_l5 %in% focus_teams,
    customer_profile == sos_line
  ) %>%
  group_by(call_responsible_department_l5) %>%
  summarise(
    sos_calls = n(),
    .groups = "drop"
  )

sos_share_by_team <- real_offered_by_team %>%
  left_join(calls_sos_by_team, by = "call_responsible_department_l5") %>%
  mutate(
    sos_calls = replace_na(sos_calls, 0),
    sos_share = if_else(total_real_offered > 0, sos_calls / total_real_offered, NA_real_)
  )

readr::write_csv(
  sos_share_by_team,
  file.path(out_dir, "real_offered_sos_share_by_team.csv")
)

plot_df <- sos_share_by_team %>%
  mutate(non_sos_calls = total_real_offered - sos_calls) %>%
  pivot_longer(cols = c(sos_calls, non_sos_calls),
               names_to = "Type", values_to = "Antal")

p_stack <- ggplot(plot_df, aes(x = call_responsible_department_l5, y = Antal, fill = Type)) +
  geom_col() +
  geom_text(
    data = sos_share_by_team,
    inherit.aes = FALSE,
    aes(
      x = call_responsible_department_l5,
      y = total_real_offered,
      label = paste0(round(sos_share * 100), "%")
    ),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(
    values = c("sos_calls" = "#D93945", "non_sos_calls" = "#3E3E3E"),
    labels = c("sos_calls" = "SOS hovedlinje", "non_sos_calls" = "Kundelinje")
  ) +
  labs(
    title = "Andel af Real Offered Calls fra SOS hovedlinje pr. team",
    x = "Team",
    y = "Antal opkald",
    fill = "Kategori"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(out_dir, "real_offered_sos_vs_customer.png"),
  p_stack,
  width = 10,
  height = 6,
  dpi = 300
)

p_share <- ggplot(sos_share_by_team, aes(x = call_responsible_department_l5, y = sos_share)) +
  geom_col(fill = "#D93945") +
  geom_text(aes(label = paste0(round(sos_share * 100), "%")),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Andel af Real Offered Calls fra SOS hovedlinje pr. team",
    x = "Team",
    y = "Andel fra SOS hovedlinje"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(out_dir, "real_offered_sos_share_by_team.png"),
  p_share,
  width = 9,
  height = 5,
  dpi = 300
)

message("Saved SOS vs customer plots to: ", out_dir)
