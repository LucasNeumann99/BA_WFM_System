# model_functions/calendar_features.R
library(tidyverse)
library(lubridate)
library(here)

# ---------------------------------------------------------
# Team → Country mapping
# ---------------------------------------------------------
team_country_map <- tibble(
  team = c(
    "Team SE 1, Travelcare",
    "Team SE 2, Travelcare",
    "Team DK, Travelcare",
    "Team NO, Travelcare",
    "Team FI, Travelcare"
  ),
  country = c("SE", "SE", "DK", "NO", "FI")
)

# ---------------------------------------------------------
# Load holiday tables
# ---------------------------------------------------------
load_holidays <- function() {
  list.files(
    here("data_raw", "holidays"),
    full.names = TRUE
  ) %>%
    map_dfr(read_csv, show_col_types = FALSE) %>%
    mutate(
      start_date = as.Date(start_date),
      end_date   = as.Date(end_date)
    )
}

# ---------------------------------------------------------
# Add holidays to time series
# ---------------------------------------------------------
add_country_holidays <- function(df, date_col = "ds") {
  
  holidays <- load_holidays()
  
  df <- df %>%
    left_join(team_country_map, by = "team") %>%
    mutate(date = as.Date(.data[[date_col]]))
  
  # Opret kolonner
  for (h in unique(holidays$holiday)) {
    df[[h]] <- FALSE
  }
  
  # Markér ferieperioder
  for (i in seq_len(nrow(holidays))) {
    h <- holidays[i, ]
    idx <- df$country == h$country &
      df$date >= h$start_date &
      df$date <= h$end_date
    df[[h$holiday]][idx] <- TRUE
  }
  
  df %>%
    mutate(
      Holiday_any = as.integer(if_any(where(is.logical), ~ .x))
    ) %>%
    select(-date)
}
