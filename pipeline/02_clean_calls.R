# pipeline/02_clean_calls.R
library(tidyverse)
library(lubridate)
library(janitor)
library(here)

# ---- paths ----
in_path  <- here("data_processed", "calls_raw.rds")
out_path <- here("data_processed", "calls_clean.rds")

# ---- load ----
calls <- readRDS(in_path)

# ---- clean & standardise ----
calls_clean <- calls %>%
  clean_names() %>%                     # svarer til gsub(" ", "_")
  mutate(
    call_start_time = dmy_hms(call_start_time, tz = "UTC")
  )

# ---- safety checks ----
stopifnot("call_start_time" %in% names(calls_clean))

# ---- save ----
saveRDS(calls_clean, out_path)

message("✔ Calls cleaned and structured")
