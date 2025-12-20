# ============================================================
# 21_optimize_shifts_v2.R
# ------------------------------------------------------------
# Formål: Lav LP-baseret vagtplanlægning ud fra scenarie-justeret Erlang-output (v2).
# Input:  <output_base>/v2/erlang/erlang_output_v2.csv
# Output: <output_base>/v2/staffing/shift_plan_optimized_v2.csv og hourly_coverage_vs_required_v2.csv
# Kræver: tidyverse, lubridate, lpSolve
# ============================================================

library(tidyverse)
library(lubridate)
library(lpSolve)
library(here)

source(here("model_functions", "paths.R"))

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
paths <- get_pipeline_paths()
base_out_dir <- file.path(paths$output, "baseline_glm")
dir.create(base_out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
prepare_staffing <- function(erlang_results,
                             use_shrinkage = TRUE,
                             hour_min = 7,
                             hour_max = 23) {
  
  required_col <- if (use_shrinkage && "agents_with_shrinkage" %in% names(erlang_results)) {
    "agents_with_shrinkage"
  } else if ("agents_required" %in% names(erlang_results)) {
    "agents_required"
  } else {
    stop("Ingen agents-kolonne fundet (forventer agents_with_shrinkage eller agents_required)")
  }
  
  erlang_results %>%
    mutate(
      ds         = as.POSIXct(ds, tz = "UTC"),
      staff_date = as.Date(ds),
      hour       = hour(ds),
      week       = isoweek(ds),
      weekday    = wday(ds, label = TRUE, week_start = 1),
      required   = .data[[required_col]]
    ) %>%
    filter(hour >= hour_min, hour < hour_max)
}

build_shift_catalog <- function(staffing_df,
                                shift_starts = 7:15,
                                shift_length = 8) {
  all_staff_dates <- sort(unique(staffing_df$staff_date))
  
  purrr::map_dfr(
    all_staff_dates,
    function(d) {
      tibble(
        staff_date = d,
        shift_id   = paste0("D", d, "_", shift_starts),
        start_hour = shift_starts,
        end_hour   = shift_starts + shift_length
      )
    }
  )
}

build_lp_matrix <- function(staffing_df, shift_catalog) {
  time_slots <- staffing_df %>%
    arrange(staff_date, hour) %>%
    mutate(row_id = row_number())
  
  coverage <- time_slots %>%
    select(staff_date, hour, row_id) %>%
    inner_join(shift_catalog, by = "staff_date", relationship = "many-to-many") %>%
    mutate(covers = if_else(hour >= start_hour & hour < end_hour, 1L, 0L)) %>%
    filter(covers == 1L)
  
  n_rows   <- nrow(time_slots)
  n_shifts <- nrow(shift_catalog)
  A <- matrix(0, nrow = n_rows, ncol = n_shifts)
  
  for (k in seq_len(nrow(coverage))) {
    i <- coverage$row_id[k]
    j <- match(coverage$shift_id[k], shift_catalog$shift_id)
    A[i, j] <- 1
  }
  
  list(
    A             = A,
    time_slots    = time_slots,
    shift_catalog = shift_catalog
  )
}

solve_shift_lp <- function(staffing_df,
                           team_name = NULL,
                           shift_starts = 7:15,
                           shift_length = 8) {
  
  shift_catalog <- build_shift_catalog(staffing_df, shift_starts, shift_length)
  lp_struct <- build_lp_matrix(staffing_df, shift_catalog)
  
  A          <- lp_struct$A
  time_slots <- lp_struct$time_slots
  shift_cat  <- lp_struct$shift_catalog
  
  rhs <- staffing_df %>%
    arrange(staff_date, hour) %>%
    pull(required)
  
  # Enkelt cost-funktion; kan vægtes efter ønsker
  shift_cost <- case_when(
    shift_cat$start_hour <= 7 ~ 100,
    shift_cat$end_hour >= 18 ~ 100,
    TRUE ~ 1
  )
  
  lp_sol <- lpSolve::lp(
    direction    = "min",
    objective.in = shift_cost,
    const.mat    = A,
    const.dir    = rep(">=", length(rhs)),
    const.rhs    = rhs,
    all.int      = TRUE
  )
  
  if (lp_sol$status != 0) {
    warning("LP fandt ikke en optimal løsning (status != 0).")
  }
  
  x_sol <- lp_sol$solution
  
  shift_plan <- shift_cat %>%
    mutate(
      agents = round(x_sol)
    ) %>%
    filter(agents > 0) %>%
    arrange(staff_date, start_hour)
  
  if (!is.null(team_name)) {
    shift_plan <- shift_plan %>%
      mutate(team = team_name)
  }
  
  coverage <- time_slots %>%
    select(staff_date, hour, row_id) %>%
    inner_join(shift_cat %>% mutate(col_id = row_number()),
               by = "staff_date",
               relationship = "many-to-many") %>%
    mutate(covers = if_else(hour >= start_hour & hour < end_hour, 1L, 0L)) %>%
    left_join(shift_plan,
              by = c("staff_date", "shift_id", "start_hour", "end_hour")) %>%
    mutate(agents = if_else(is.na(agents), 0L, agents)) %>%
    group_by(staff_date, hour) %>%
    summarise(
      staffed_agents = sum(agents * covers),
      .groups = "drop"
    ) %>%
    left_join(staffing_df, by = c("staff_date", "hour")) %>%
    arrange(staff_date, hour)
  
  if (!is.null(team_name)) {
    coverage <- coverage %>%
      mutate(team = team_name)
  }
  
  list(
    lp_obj       = lp_sol$objval,
    shift_plan   = shift_plan,
    hourly_cover = coverage
  )
}

# ------------------------------------------------------------
# Main
# ------------------------------------------------------------
in_files <- list.files(
  base_out_dir,
  pattern = "erlang_output_v2.csv",
  recursive = TRUE,
  full.names = TRUE
)

if (length(in_files) == 0) {
  stop("Ingen erlang_output_v2.csv fundet under: ", base_out_dir)
}

for (in_path in in_files) {
  team_name <- basename(dirname(dirname(in_path)))
  erlang_results <- read_csv(in_path, show_col_types = FALSE)
  if ("team" %in% names(erlang_results)) {
    erlang_results <- erlang_results %>%
      filter(team == team_name)
  }
  
  staffing_df <- prepare_staffing(erlang_results, use_shrinkage = TRUE)
  solution <- solve_shift_lp(staffing_df, team_name = team_name)
  
  shift_plan <- solution$shift_plan %>%
    select(-shift_id)
  
  hourly_cover <- solution$hourly_cover
  
  out_dir  <- file.path(base_out_dir, team_name, "staffing")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_plan <- file.path(out_dir, "shift_plan_optimized_v2.csv")
  out_cov  <- file.path(out_dir, "hourly_coverage_vs_required_v2.csv")
  
  write_csv(shift_plan, out_plan)
  write_csv(hourly_cover, out_cov)
  
  message("✔ Shift plan gemt: ", out_plan)
  message("✔ Hourly coverage gemt: ", out_cov)
}
