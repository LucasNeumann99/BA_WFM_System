# ============================================================
# 01_team_top5_customer_trends.R
# ------------------------------------------------------------
# Formål:
# - For hvert BA-team (DK, FI, NO, SE1, SE2) finde de 5 største
#   service_entrances (målt på samlet volumen 2022–2025)
# - Plotte månedlig udvikling i antal opkald for de 5 største
# - Vise anonymiserede "customer_codes" i legend i stedet for navne
#   - Storebrand får altid koden 370
#
# Output:
# - 5 PNG-filer (én pr. team) i:
#   figures/diagnostics/team_top5/
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

# ------------------------------------------------------------
# 1) Læs data
# ------------------------------------------------------------
calls <- readRDS(here("data_processed", "calls_clean.rds"))

# De teams vi arbejder med i BA'en
focus_teams <- c(
  "Team DK 1, Travelcare",
  "Team FI 1, Travelcare",
  "Team NO 1, Travelcare",
  "Team SE 1, Travelcare",
  "Team SE 2, Travelcare"
)

message("Unikke teams i datasættet:")
print(sort(unique(calls$call_responsible_department_l5)))

# ------------------------------------------------------------
# 2) Helper: safe filnavn fra team-navn
# ------------------------------------------------------------
make_safe_name <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

# ------------------------------------------------------------
# 3) Output-mappe
# ------------------------------------------------------------
fig_dir <- here("figures", "diagnostics", "team_top5")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4) Loop over teams
# ------------------------------------------------------------
for (tm in focus_teams) {
  message("------------------------------------------------------------")
  message("Bearbejder team: ", tm)
  
  df_team <- calls %>%
    filter(call_responsible_department_l5 == tm) %>%
    mutate(
      date  = as.Date(call_start_time),
      month = floor_date(date, "month")
    )
  
  if (nrow(df_team) == 0) {
    warning("Ingen data fundet for team: ", tm)
    next
  }
  
  # 4a) Månedsvolumen pr. service_entrance
  monthly_by_ent <- df_team %>%
    filter(!is.na(service_entrance)) %>%
    group_by(service_entrance, month) %>%
    summarise(calls = n(), .groups = "drop")
  
  # 4b) Find top 5 service_entrances (samlet volumen)
  top5_ents <- monthly_by_ent %>%
    group_by(service_entrance) %>%
    summarise(calls_total = sum(calls), .groups = "drop") %>%
    arrange(desc(calls_total)) %>%
    slice_head(n = 5)
  
  message("Top 5 service_entrances for ", tm, ":")
  print(top5_ents)
  
  # 4c) Anonymiser: customer_code
  # - Hvis service_entrance indeholder "Storebrand" -> 370
  # - Ellers: 310, 320, 330, ...
  anon_map <- top5_ents %>%
    mutate(
      customer_code = dplyr::case_when(
        stringr::str_detect(service_entrance, regex("Storebrand", ignore_case = TRUE)) ~ 370L,
        TRUE ~ 300L + row_number() * 10L
      )
    )
  
  # Data til plot
  plot_data <- monthly_by_ent %>%
    semi_join(anon_map, by = "service_entrance") %>%
    left_join(anon_map %>% select(service_entrance, customer_code),
              by = "service_entrance")
  
  # 4d) Plot
  p <- ggplot(plot_data,
              aes(x = month, y = calls, colour = factor(customer_code))) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    labs(
      title = paste0("Top 5 kunder – ", tm, " (månedlig volumen 2022–2025)"),
      x = "Måned",
      y = "Antal opkald",
      colour = "Customer code"
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # 4e) Gem figur
  file_safe_team <- make_safe_name(tm)
  out_path <- file.path(fig_dir,
                        paste0("top5_trend_", file_safe_team, ".png"))
  
  ggsave(
    filename = out_path,
    plot     = p,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  message("✔ Figur gemt: ", out_path)
}

message("✔ Alle team-plots genereret.")

