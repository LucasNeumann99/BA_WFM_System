# ============================================================
# 02_team_no_customer_shift.R
# ------------------------------------------------------------
# Formål:
# - Analysere strukturelle skift i volume på Team NO
# - Identificere service_entrances med stort fald 2024 -> 2025
# - Særligt fokus på customer_code = 370 (anonymiseret kunde)
#
# Output:
# - Konsol-print af kandidater
# - CSV: <output_base>/analysis_extra/team_no_customer_shift_candidates.csv
# - Plots: <output_base>/analysis_extra/*.png (kun ved interaktiv kørsel)
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))

# ------------------------------------------------------------
# 1) Læs data og filtrér til Team NO
# ------------------------------------------------------------
team_no        <- "Team NO 1, Travelcare"
storebrand_code <- 370L   # anonymiseret kunde-id

calls <- readRDS(here("data_processed", "calls_clean.rds"))

no_calls <- calls %>%
  filter(call_responsible_department_l5 == team_no) %>%
  mutate(
    date = as.Date(call_start_time),
    year = year(date)
  )

message("Antal Team NO-opkald i datasættet: ", nrow(no_calls))

# ------------------------------------------------------------
# 2) Dagligt og årligt volumen pr. service_entrance
# ------------------------------------------------------------
no_daily_by_ent <- no_calls %>%
  filter(!is.na(service_entrance)) %>%
  group_by(service_entrance, date, year) %>%
  summarise(calls = n(), .groups = "drop")

no_yearly_by_ent <- no_daily_by_ent %>%
  group_by(service_entrance, year) %>%
  summarise(calls = sum(calls), .groups = "drop")

# Årlige totals for hele Team NO
no_yearly_team <- no_yearly_by_ent %>%
  group_by(year) %>%
  summarise(team_calls = sum(calls), .groups = "drop")

# ------------------------------------------------------------
# 3) Udregn shares + indeks pr. service_entrance
# ------------------------------------------------------------
no_yearly <- no_yearly_by_ent %>%
  left_join(no_yearly_team, by = "year") %>%
  group_by(service_entrance) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    share           = calls / team_calls,        # andel af teamets volume
    calls_prev      = lag(calls),
    share_prev      = lag(share),
    yoy_ratio       = calls / calls_prev,        # år/år forhold
    yoy_change      = calls - calls_prev,
    base_calls      = first(calls),
    calls_index_100 = 100 * calls / base_calls
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 4) Find kandidater med stort fald 2024 -> 2025
# ------------------------------------------------------------
min_calls_prev  <- 1000   # min antal kald i 2024
min_share_prev  <- 0.05   # min 5 % af Team NO i 2024
max_ratio_24_25 <- 0.6    # max 60 % af 2024-niveau i 2025

candidates_24_25 <- no_yearly %>%
  filter(year %in% c(2024, 2025)) %>%
  dplyr::select(service_entrance, year, calls, share) %>%
  tidyr::pivot_wider(
    names_from  = year,
    values_from = c(calls, share),
    names_glue  = "{.value}_{year}"
  ) %>%
  mutate(
    calls_24 = replace_na(calls_2024, 0L),
    calls_25 = replace_na(calls_2025, 0L),
    share_24 = replace_na(share_2024, 0),
    share_25 = replace_na(share_2025, 0)
  ) %>%
  filter(
    calls_24 >= min_calls_prev,
    share_24 >= min_share_prev,
    calls_25 / calls_24 <= max_ratio_24_25
  ) %>%
  mutate(
    drop_ratio   = calls_25 / calls_24,
    drop_abs     = calls_25 - calls_24,
    drop_team_pp = (share_25 - share_24) * 100
  ) %>%
  arrange(drop_ratio)

message("\nKandidater med stort fald 2024 -> 2025 (Team NO):")
print(candidates_24_25, n = Inf)

# ------------------------------------------------------------
# 5) YTD-sammenligning 2024 vs 2025 (klip til samme dag)
# ------------------------------------------------------------
max_yday_2025 <- no_calls %>%
  filter(year(date) == 2025) %>%
  summarise(max_yday = max(yday(date))) %>%
  pull(max_yday)

no_calls_ytd <- no_calls %>%
  mutate(
    year = year(date),
    yday = yday(date)
  ) %>%
  filter(yday <= max_yday_2025)

no_yearly_team_ytd <- no_calls_ytd %>%
  group_by(year) %>%
  summarise(team_calls = n(), .groups = "drop")

message("\nTeam NO – YTD volume pr. år til dag ", max_yday_2025, ":")
print(no_yearly_team_ytd)

# ------------------------------------------------------------
# 6) Specifikt fokus: “Storebrand” (anonymiseret som customer_code = 370)
# ------------------------------------------------------------
storebrand_code    <- 370L
storebrand_pattern <- "Storebrand"   # vi matcher på navn i service_entrance
year_colors <- c(
  "#D93945", # SOS_red
  "#3E3E3E", # Gray_dark
  "#2A6F97", # Blue
  "#2E9A5D", # Green
  "#8E5C9E", # Purple
  "#F46A74", # SOS_light
  "#E3B23C"  # Yellow
)

# Månedlig udvikling for alle service_entrances der indeholder "Storebrand"
store_monthly_by_year <- no_calls %>%
  filter(str_detect(service_entrance, storebrand_pattern)) %>%
  mutate(
    customer_code = storebrand_code,
    year  = year(date),
    month = month(date)
  ) %>%
  group_by(customer_code, year, month) %>%
  summarise(calls = n(), .groups = "drop")

# (valgfrit) plot – kun ved interaktiv kørsel
if (interactive() && nrow(store_monthly_by_year) > 0) {
  p_store_monthly <- ggplot(
    store_monthly_by_year %>% filter(year >= 2023),
    aes(x = factor(month), y = calls, group = year, colour = factor(year))
  ) +
    geom_line() +
    geom_point(size = 2) +
    labs(
      title  = "Månedlig udvikling i customer_code 370 (Storebrand, Team NO)",
      x      = "Måned",
      y      = "Antal opkald",
      colour = "År"
    ) +
    scale_colour_manual(values = year_colors) +
    scale_x_discrete(labels = 1:12) +
    theme_minimal()
  
  print(p_store_monthly)
}

# ------------------------------------------------------------
# 7) Gem et lille summary til rapport
# ------------------------------------------------------------
diag_dir <- file.path(
  get_pipeline_paths()$output,
  "analysis_extra"
)
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

summary_out <- candidates_24_25 %>%
  mutate(
    # anonymiser Storebrand i output – men brug IKKE customer_code til at filtrere input
    customer_code = if_else(
      str_detect(service_entrance, storebrand_pattern),
      storebrand_code,
      NA_integer_
    )
  )

readr::write_csv(
  summary_out,
  file.path(diag_dir, "team_no_customer_shift_candidates.csv")
)

message(
  "✔ Diagnostics gemt i: ",
  file.path(diag_dir, "team_no_customer_shift_candidates.csv")
)
