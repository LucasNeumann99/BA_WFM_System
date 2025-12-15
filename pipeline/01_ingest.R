# pipeline/01_ingest.R
library(tidyverse)
library(here)

# ---- paths ----
raw_calls_path <- here("data_raw", "service_calls.csv")
out_path       <- here("data_processed", "calls_raw.rds")

# ---- ingest ----
calls_raw <- read_delim(
  raw_calls_path,
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

# ---- sanity check ----
if (ncol(calls_raw) == 1) {
  stop("⚠️ Data er stadig én kolonne – tjek delimiter eller filformat")
}

# ---- save ----
saveRDS(calls_raw, out_path)

message("✔ Raw calls ingested and saved")
