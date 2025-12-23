# ============================================================
# 05_shrinkage_staffing_scenarios.R
# ------------------------------------------------------------
# Formål:
# - Kør 3 shrinkage-scenarier (0.30, 0.35, 0.40) på Erlang-output
# - Generer staffing pr. team pr. scenarie
# - Lav samlet Travelcare månedsoversigt (2026) + plot
#
# Output:
# - results/v2/shrinkage_scenarios/<scenario>/erlang_input/<team>/erlang_input_v2.csv
# - results/v2/shrinkage_scenarios/<scenario>/erlang_output/<team>/erlang_output_v2.csv
# - results/v2/shrinkage_scenarios/<scenario>/staffing/<team>/shift_plan_optimized_v2.csv
# - results/v2/shrinkage_scenarios/<scenario>/staffing/<team>/hourly_coverage_vs_required_v2.csv
# - results/v2/shrinkage_scenarios/summary/shift_plans_all.rds
# - results/v2/shrinkage_scenarios/summary/monthly_hours_2026.rds
# - output/baseline_glm/shrinkage_hours_2026_monthly.csv
# - output/baseline_glm/shrinkage_hours_2026_monthly.png
# ============================================================

library(tidyverse)
library(lubridate)
library(lpSolve)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
base_out_dir <- file.path(paths$output, "baseline_glm")
results_base <- file.path(paths$results, "v2", "shrinkage_scenarios")
summary_dir  <- file.path(results_base, "summary")

dir.create(results_base, recursive = TRUE, showWarnings = FALSE)
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

shrinkages <- c(0.25, 0.30, 0.35)

input_files <- list.files(
  base_out_dir,
  pattern = "erlang_output_v2.csv",
  recursive = TRUE,
  full.names = TRUE
)

if (length(input_files) == 0) {
  stop("Ingen erlang_output_v2.csv fundet under: ", base_out_dir)
}

# ------------------------------------------------------------
# Helpers (samme logik som pipeline/21_optimize_shifts_v2.R)
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
  
  shift_cost <- case_when(
    shift_cat$start_hour <= 7 ~ 100,
    shift_cat$end_hour >= 18 ~ 100,
    TRUE ~ 1
  )
  
  lp_sol <- lpSolve::lp(
    direction = "min",
    objective.in = shift_cost,
    const.mat = A,
    const.dir = rep(">=", length(rhs)),
    const.rhs = rhs,
    all.int = TRUE
  )
  
  shift_plan <- shift_cat %>%
    mutate(agents = lp_sol$solution) %>%
    filter(agents > 0)
  
  coverage <- time_slots %>%
    select(staff_date, hour, row_id) %>%
    inner_join(shift_cat, by = "staff_date", relationship = "many-to-many") %>%
    mutate(covers = if_else(hour >= start_hour & hour < end_hour, 1L, 0L)) %>%
    filter(covers == 1L) %>%
    mutate(agents = lp_sol$solution[match(shift_id, shift_cat$shift_id)]) %>%
    group_by(staff_date, hour) %>%
    summarise(agents = sum(agents), .groups = "drop") %>%
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

shift_plans_all <- list()

for (shr in shrinkages) {
  scenario_tag <- sprintf("shrinkage_%0.2f", shr)
  scenario_dir <- file.path(results_base, scenario_tag)
  
  erlang_input_dir  <- file.path(scenario_dir, "erlang_input")
  erlang_output_dir <- file.path(scenario_dir, "erlang_output")
  staffing_dir      <- file.path(scenario_dir, "staffing")
  
  dir.create(erlang_input_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(erlang_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(staffing_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (out_path in input_files) {
    team_name <- basename(dirname(dirname(out_path)))
    
    # Erlang output (baseline) + recompute shrinkage agents
    erlang_base <- read_csv(out_path, show_col_types = FALSE)
    if ("team" %in% names(erlang_base)) {
      erlang_base <- erlang_base %>% filter(team == team_name)
    }
    
    erlang_adj <- erlang_base %>%
      mutate(
        shrinkage = shr,
        agents_with_shrinkage = if_else(
          is.na(agents_required),
          NA_real_,
          ceiling(agents_required / (1 - shrinkage))
        )
      )
    
    out_erlang_dir <- file.path(erlang_output_dir, team_name)
    dir.create(out_erlang_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv(erlang_adj, file.path(out_erlang_dir, "erlang_output_v2.csv"))
    
    # Erlang input (baseline) with updated shrinkage
    input_path <- file.path(dirname(out_path), "erlang_input_v2.csv")
    if (file.exists(input_path)) {
      erlang_input <- read_csv(input_path, show_col_types = FALSE) %>%
        mutate(shrinkage = shr)
    } else {
      erlang_input <- erlang_adj %>%
        select(
          team, ds, calls, aht_sec, target_sl, threshold_sec,
          shrinkage, model_used, scenario_label, run_id
        )
    }
    
    out_input_dir <- file.path(erlang_input_dir, team_name)
    dir.create(out_input_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv(erlang_input, file.path(out_input_dir, "erlang_input_v2.csv"))
    
    # Staffing
    staffing_df <- prepare_staffing(erlang_adj, use_shrinkage = TRUE)
    solution <- solve_shift_lp(staffing_df, team_name = team_name)
    
    shift_plan <- solution$shift_plan %>% select(-shift_id)
    hourly_cover <- solution$hourly_cover
    
    out_staff_dir <- file.path(staffing_dir, team_name)
    dir.create(out_staff_dir, recursive = TRUE, showWarnings = FALSE)
    
    write_csv(shift_plan, file.path(out_staff_dir, "shift_plan_optimized_v2.csv"))
    write_csv(hourly_cover, file.path(out_staff_dir, "hourly_coverage_vs_required_v2.csv"))
    
    shift_plans_all[[length(shift_plans_all) + 1]] <- shift_plan %>%
      mutate(
        team = team_name,
        shrinkage = shr
      )
  }
}

shift_plans_all <- bind_rows(shift_plans_all)
saveRDS(shift_plans_all, file.path(summary_dir, "shift_plans_all.rds"))

monthly_2026 <- shift_plans_all %>%
  mutate(
    staff_date = as.Date(staff_date),
    month = floor_date(staff_date, "month"),
    hours = (end_hour - start_hour) * agents
  ) %>%
  filter(year(staff_date) == 2026) %>%
  group_by(shrinkage, month) %>%
  summarise(hours = sum(hours), .groups = "drop") %>%
  arrange(shrinkage, month)

saveRDS(monthly_2026, file.path(summary_dir, "monthly_hours_2026.rds"))

out_csv <- file.path(base_out_dir, "shrinkage_hours_2026_monthly.csv")
write_csv(monthly_2026, out_csv)

total_2026 <- monthly_2026 %>%
  group_by(shrinkage) %>%
  summarise(total_hours = sum(hours), .groups = "drop") %>%
  arrange(shrinkage)

out_total_csv <- file.path(base_out_dir, "shrinkage_hours_2026_total.csv")
write_csv(total_2026, out_total_csv)

plot_df <- monthly_2026 %>%
  mutate(
    shrinkage_label = sprintf("%.2f", shrinkage),
    shrinkage_label = factor(shrinkage_label, levels = sprintf("%.2f", shrinkages))
  )

p <- ggplot(plot_df, aes(month, hours, colour = shrinkage_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_colour_manual(values = c(
    "0.25" = "#D93945", # SOS_red
    "0.30" = "#2A6F97", # Blue
    "0.35" = "#2E9A5D"  # Green
  )) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Travelcare: Staff hours per month (2026) by shrinkage",
    subtitle = "Total staffing hours after shrinkage adjustment",
    x = "Month",
    y = "Hours",
    colour = "Shrinkage"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

out_plot <- file.path(base_out_dir, "shrinkage_hours_2026_monthly.png")
ggsave(out_plot, plot = p, width = 10, height = 5, dpi = 300)

message("✔ Monthly table: ", out_csv)
message("✔ Total table: ", out_total_csv)
message("✔ Plot: ", out_plot)
