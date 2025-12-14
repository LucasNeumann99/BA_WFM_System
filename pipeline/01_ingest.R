# pipeline/01_ingest.R
library(tidyverse)
library(here)

# ---- paths ----
raw_calls_path <- here("data_raw", "service_calls.csv")
out_path       <- here("data_processed", "calls_raw.rds")

# ---- ingest ----
calls_raw <- read_csv(
  raw_calls_path,
  show_col_types = FALSE
)

# ---- save ----
saveRDS(calls_raw, out_path)

message("✔ Raw calls ingested and saved")
