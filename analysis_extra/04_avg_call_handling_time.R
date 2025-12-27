# ============================================================
# 04_avg_call_handling_time.R
# ------------------------------------------------------------
# Formål:
# - Opsummere avg_call_handling_time_sec pr. team + samlet
# - Kun real_offered_call (TRUE/1)
#
# Output:
# - <output_base>/Manning/extra_analytics/Erlang_C/avg_call_handling_time_sec_by_team.csv
# ============================================================

library(tidyverse)
library(here)

source(here("model_functions", "paths.R"))
calls <- readRDS(here("data_processed", "calls_clean.rds"))

if (!"avg_call_handling_time_sec" %in% names(calls)) {
  stop("avg_call_handling_time_sec not found in calls_clean.rds")
}
if (!"real_offered_call" %in% names(calls)) {
  stop("real_offered_call not found in calls_clean.rds")
}

roc <- calls$real_offered_call
roc_keep <- if (is.logical(roc)) {
  roc %in% TRUE
} else if (is.numeric(roc)) {
  roc == 1
} else {
  toupper(as.character(roc)) %in% c("T", "TRUE", "1", "Y", "YES")
}

df <- calls %>%
  mutate(roc_keep = roc_keep) %>%
  filter(roc_keep)

team_col <- if ("call_responsible_department_l5" %in% names(df)) {
  "call_responsible_department_l5"
} else if ("team" %in% names(df)) {
  "team"
} else {
  stop("No team column found (expected call_responsible_department_l5 or team).")
}

df <- df %>% mutate(team = .data[[team_col]])

summarise_aht <- function(d) {
  d %>%
    summarise(
      n = sum(!is.na(avg_call_handling_time_sec)),
      mean = mean(avg_call_handling_time_sec, na.rm = TRUE),
      median = median(avg_call_handling_time_sec, na.rm = TRUE),
      sd = sd(avg_call_handling_time_sec, na.rm = TRUE),
      p10 = quantile(avg_call_handling_time_sec, 0.10, na.rm = TRUE),
      p90 = quantile(avg_call_handling_time_sec, 0.90, na.rm = TRUE)
    )
}

per_team <- df %>%
  group_by(team) %>%
  summarise_aht() %>%
  ungroup()

overall <- df %>%
  summarise_aht() %>%
  mutate(team = "ALL")

out <- bind_rows(per_team, overall) %>%
  dplyr::select(team, everything())

out_dir <- file.path(
  get_pipeline_paths()$output,
  "Manning",
  "extra_analytics",
  "Erlang_C"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "avg_call_handling_time_sec_by_team.csv")

readr::write_csv(out, out_path)
message("✔ Saved: ", out_path)
