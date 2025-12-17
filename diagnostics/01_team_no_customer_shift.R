# ============================================================
# diagnostics/01_team_no_customer_shift.R
# ------------------------------------------------------------
# Formål:
# - Analysere strukturelle skift i volume på Team NO
# - Identificere service_entrances med stort fald 2024 -> 2025
# - Særligt fokus på customer_code = 370 (anonymiseret kunde)
#
# Output:
# - Konsol-print af kandidater
# - CSV: results/diagnostics/team_no_customer_shift_candidates.csv
# - Plots: figures/diagnostics/team_no/*.png
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

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
  select(service_entrance, year, calls, share) %>%
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
# 6) Specifikt fokus: customer_code = 370
# ------------------------------------------------------------
store_monthly_by_year <- no_calls %>%
  filter(customer_code == storebrand_code) %>%
  mutate(
    year  = year(date),
    month = month(date)
  ) %>%
  group_by(year, month) %>%
  summarise(calls = n(), .groups = "drop")

message("\nMånedlig udvikling for customer_code 370 (aggreggeret):")
print(store_monthly_by_year %>% arrange(year, month))

# ------------------------------------------------------------
# 7) Gem summary + plots til BA-dokumentation
# ------------------------------------------------------------
diag_dir <- here("results", "diagnostics")
fig_dir  <- here("figures", "diagnostics", "team_no")

dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)

# Map service_entrance -> mest hyppige customer_code
service_code_map <- no_calls %>%
  group_by(service_entrance, customer_code) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(service_entrance, customer_code)

summary_out <- candidates_24_25 %>%
  left_join(service_code_map, by = "service_entrance") %>%
  select(
    customer_code,
    service_entrance,
    calls_24, calls_25,
    share_24, share_25,
    drop_ratio, drop_abs, drop_team_pp
  )

readr::write_csv(
  summary_out,
  file.path(diag_dir, "team_no_customer_shift_candidates.csv")
)

message("\n✔ Diagnostics gemt i: results/diagnostics/team_no_customer_shift_candidates.csv")

# Plots – kun med anonymiseret label
p_store_calls <- ggplot(
  store_monthly_by_year %>% filter(year >= 2023),
  aes(x = factor(month), y = calls, group = factor(year), colour = factor(year))
) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Månedlig udvikling i opkald – customer_code 370 (Team NO)",
    x = "Måned",
    y = "Antal opkald",
    colour = "År"
  ) +
  scale_x_discrete(labels = 1:12) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "customer_370_monthly_calls.png"),
  plot     = p_store_calls,
  width    = 9,
  height   = 5,
  dpi      = 300
)

# Team NO totalt pr. måned (for at se andel)
team_monthly <- no_calls %>%
  mutate(month = floor_date(date, "month")) %>%
  count(month, name = "team_calls")

store_monthly <- no_calls %>%
  filter(customer_code == storebrand_code) %>%
  mutate(month = floor_date(date, "month")) %>%
  count(month, name = "calls_370")

store_share_monthly <- store_monthly %>%
  left_join(team_monthly, by = "month") %>%
  mutate(share_370 = calls_370 / team_calls)

p_store_share <- ggplot(store_share_monthly, aes(x = month, y = share_370 * 100)) +
  geom_line() +
  geom_point() +
  labs(
    title = "customer_code 370 – andel af Team NO pr. måned",
    x = "Måned",
    y = "Andel af Team NO (%)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "customer_370_share_of_team_no.png"),
  plot     = p_store_share,
  width    = 9,
  height   = 5,
  dpi      = 300
)

message("✔ Plots gemt i: figures/diagnostics/team_no/")
