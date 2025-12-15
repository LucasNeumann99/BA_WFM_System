library(tidyverse)
library(lubridate)
library(here)

# --------------------------------------------------
# Generic holiday loader (ALL countries)
# --------------------------------------------------
add_country_holidays <- function(df, country_code, date_col = "ds") {
  
  # ---- load holidays for given country ----
  path <- here(
    "data_raw", "holidays",
    paste0("holidays_", country_code, ".csv")
  )
  
  holidays <- readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(
      start_date = as.Date(start_date),
      end_date   = as.Date(end_date)
    )
  
  # ---- extract dates from df ----
  dates <- as.Date(df[[date_col]])
  
  # ---- initialise holiday flags ----
  holiday_types <- unique(holidays$holiday)
  
  for (h in holiday_types) {
    df[[h]] <- FALSE
  }
  
  # ---- mark intervals ----
  for (i in seq_len(nrow(holidays))) {
    idx <- which(
      dates >= holidays$start_date[i] &
        dates <= holidays$end_date[i]
    )
    df[idx, holidays$holiday[i]] <- TRUE
  }
  
  # ---- combined holiday flag ----
  df <- df %>%
    mutate(
      Holiday_any = as.integer(
        if_any(all_of(holiday_types), identity)
      )
    )
  
  return(df)
}
