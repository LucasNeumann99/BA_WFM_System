# ============================================================
# 21_optimize_shifts_v2.R
# ------------------------------------------------------------
# Formål: Lav LP-baseret vagtplanlægning ud fra scenarie-justeret Erlang-output (v2).
# Input:  <output_base>/Manning/<team>/erlang/erlang_output_v2.csv
# Output: <output_base>/Manning/<team>/staffing/shift_plan_optimized_v2.csv og hourly_coverage_vs_required_v2.csv
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
base_out_dir <- file.path(paths$output, "Manning")
dir.create(base_out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
prepare_staffing <- function(erlang_results,
                             use_shrinkage = TRUE,
                             hour_min = 0,
                             hour_max = 24,
                             night_hours = c(22, 23, 0, 1, 2, 3, 4)) {
  
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
    filter(hour >= hour_min, hour < hour_max) %>%
    mutate(
      required = if_else(hour %in% night_hours, 1, required)
    )
}

build_shift_catalog <- function(staffing_df,
                                shift_starts = 5:15,
                                shift_length = 8,
                                night_start = 22,
                                night_length = 9) {
  all_staff_dates <- sort(unique(staffing_df$staff_date))
  
  shift_catalog <- purrr::map_dfr(
    all_staff_dates,
    function(d) {
      day_shifts <- tibble(
        staff_date = d,
        shift_id   = paste0("D", d, "_", shift_starts),
        start_hour = shift_starts,
        end_hour   = shift_starts + shift_length
      )
      night_shift <- tibble(
        staff_date = d,
        shift_id   = paste0("N", d, "_", night_start),
        start_hour = night_start,
        end_hour   = night_start + night_length
      )
      bind_rows(day_shifts, night_shift)
    }
  )
  
  # Add a night shift for the day before the first date to cover early hours
  if (length(all_staff_dates) > 0) {
    first_day <- min(all_staff_dates)
    pre_night <- tibble(
      staff_date = first_day - days(1),
      shift_id   = paste0("N", first_day - days(1), "_", night_start),
      start_hour = night_start,
      end_hour   = night_start + night_length
    )
    shift_catalog <- bind_rows(shift_catalog, pre_night)
  }
  
  shift_catalog
}

build_lp_matrix <- function(staffing_df, shift_catalog) {
  time_slots <- staffing_df %>%
    arrange(staff_date, hour) %>%
    mutate(row_id = row_number())
  
  day_part <- shift_catalog %>%
    mutate(
      cover_date = staff_date,
      start_cover = start_hour,
      end_cover = pmin(end_hour, 24)
    )
  
  night_part <- shift_catalog %>%
    filter(end_hour > 24) %>%
    mutate(
      cover_date = staff_date + days(1),
      start_cover = 0,
      end_cover = end_hour - 24
    )
  
  coverage <- bind_rows(day_part, night_part) %>%
    inner_join(time_slots, by = c("cover_date" = "staff_date"), relationship = "many-to-many") %>%
    mutate(covers = hour >= start_cover & hour < end_cover) %>%
    filter(covers) %>%
    mutate(covers = 1L)
  
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
                           shift_starts = 5:15,
                           shift_length = 8,
                           night_start = 22,
                           night_length = 9,
                           night_strict_hours = c(23, 0, 1, 2, 3, 4)) {
  
  shift_catalog <- build_shift_catalog(
    staffing_df,
    shift_starts = shift_starts,
    shift_length = shift_length,
    night_start = night_start,
    night_length = night_length
  )
  lp_struct <- build_lp_matrix(staffing_df, shift_catalog)
  
  A          <- lp_struct$A
  time_slots <- lp_struct$time_slots
  shift_cat  <- lp_struct$shift_catalog
  
  rhs <- staffing_df %>%
    arrange(staff_date, hour) %>%
    pull(required)
  
  # Enkelt cost-funktion; kan vægtes efter ønsker
  shift_cost <- case_when(
    shift_cat$start_hour == night_start ~ 300,
    shift_cat$start_hour <= 7 ~ 100,
    shift_cat$end_hour >= 18 ~ 100,
    TRUE ~ 1
  )

  # Add strict night constraints (exactly 1 between 23-05)
  night_rows <- time_slots$row_id[time_slots$hour %in% night_strict_hours]
  if (length(night_rows) > 0) {
    A_night <- A[night_rows, , drop = FALSE]
    A <- rbind(A, A_night, A_night)
    const_dir <- c(rep(">=", length(rhs)), rep("<=", length(night_rows)), rep(">=", length(night_rows)))
    const_rhs <- c(rhs, rep(1, length(night_rows)), rep(1, length(night_rows)))
  } else {
    const_dir <- rep(">=", length(rhs))
    const_rhs <- rhs
  }
  
  lp_sol <- lpSolve::lp(
    direction    = "min",
    objective.in = shift_cost,
    const.mat    = A,
    const.dir    = const_dir,
    const.rhs    = const_rhs,
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
  
  day_part <- shift_cat %>%
    mutate(
      cover_date = staff_date,
      start_cover = start_hour,
      end_cover = pmin(end_hour, 24)
    )
  
  night_part <- shift_cat %>%
    filter(end_hour > 24) %>%
    mutate(
      cover_date = staff_date + days(1),
      start_cover = 0,
      end_cover = end_hour - 24
    )
  
  coverage <- bind_rows(day_part, night_part) %>%
    inner_join(time_slots %>% select(staff_date, hour),
               by = c("cover_date" = "staff_date"),
               relationship = "many-to-many") %>%
    mutate(covers = if_else(hour >= start_cover & hour < end_cover, 1L, 0L)) %>%
    filter(covers == 1L) %>%
    left_join(shift_plan,
              by = c("staff_date", "shift_id", "start_hour", "end_hour")) %>%
    mutate(agents = if_else(is.na(agents), 0L, agents)) %>%
    group_by(cover_date, hour) %>%
    summarise(
      staffed_agents = sum(agents),
      .groups = "drop"
    ) %>%
    rename(staff_date = cover_date) %>%
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
in_files <- in_files[!grepl("/summary/", in_files, fixed = TRUE)]

if (length(in_files) == 0) {
  stop("Ingen erlang_output_v2.csv fundet under: ", base_out_dir)
}

for (in_path in in_files) {
  team_name <- basename(dirname(dirname(in_path)))
  if (team_name == "Team SE total") {
    message("Springer over staffing for Team SE total.")
    next
  }
  erlang_results <- read_csv(in_path, show_col_types = FALSE)
  if ("team" %in% names(erlang_results)) {
    erlang_results <- erlang_results %>%
      filter(team == team_name)
  }
  
  staffing_df <- prepare_staffing(erlang_results, use_shrinkage = TRUE)
  solution <- solve_shift_lp(staffing_df, team_name = team_name)
  
  shift_plan <- solution$shift_plan %>%
    select(-shift_id)
  
  shift_plan_out <- shift_plan %>%
    mutate(
      end_date = if_else(end_hour > 24, staff_date + days(1), staff_date),
      end_hour = if_else(end_hour > 24, end_hour - 24, end_hour)
    )
  
  hourly_cover <- solution$hourly_cover
  
  out_dir  <- file.path(base_out_dir, team_name, "staffing")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_plan <- file.path(out_dir, "shift_plan_optimized_v2.csv")
  out_cov  <- file.path(out_dir, "hourly_coverage_vs_required_v2.csv")
  
  write_csv(shift_plan_out, out_plan)
  write_csv(hourly_cover, out_cov)
  
  message("✔ Shift plan gemt: ", out_plan)
  message("✔ Hourly coverage gemt: ", out_cov)
}
