# pipeline/02_clean_calls.R
library(tidyverse)
library(lubridate)
library(here)

# ---- paths ----
in_path  <- here("data_processed", "calls_raw.rds")
out_path <- here("data_processed", "calls_clean.rds")

# ---- load ----
calls <- readRDS(in_path)

# ---- clean ----
calls_clean <- calls %>%
  janitor::clean_names() %>%
  mutate(
    call_start_time = dmy_hms(call_start_time, tz = "UTC")
  )

# ---- save ----
saveRDS(calls_clean, out_path)

message("✔ Calls cleaned and saved")
