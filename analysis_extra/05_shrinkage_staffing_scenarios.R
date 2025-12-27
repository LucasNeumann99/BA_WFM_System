# ============================================================
# 05_shrinkage_staffing_scenarios.R
# ------------------------------------------------------------
# Formål:
# - Kør shrinkage-scenarier på eksisterende operative Erlang-output
# - Lav samlet Travelcare månedsoversigt (2026) + plot
# - Ingen staffing/shift-plan outputs i scenarier (kun overblik)
#
# Output:
# - <output_base>/Manning/extra_analytics/shrinkage_hours_2026_monthly.csv
# - <output_base>/Manning/extra_analytics/shrinkage_hours_2026_monthly.png
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
base_out_dir <- file.path(paths$output, "Manning")
shrinkages <- c(0.30, 0.35, 0.40)

input_files <- list.files(
  base_out_dir,
  pattern = "erlang_output_v2.csv",
  recursive = TRUE,
  full.names = TRUE
)

# Kun baseline erlang-output (undgå summary/scenarier)
input_files <- input_files[!grepl("/summary/", input_files, fixed = TRUE)]
input_files <- input_files[!grepl("/shrinkage_scenarios/", input_files, fixed = TRUE)]

if (length(input_files) == 0) {
  stop("Ingen erlang_output_v2.csv fundet under: ", base_out_dir)
}

hours_all <- list()

for (shr in shrinkages) {
  scenario_tag <- sprintf("shrinkage_%0.2f", shr)
  scenario_dir <- file.path(paths$output, "Manning", "extra_analytics", "shrinkage_scenarios", scenario_tag)
  dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)
  
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

    hours_all[[length(hours_all) + 1]] <- erlang_adj %>%
      transmute(
        team = team_name,
        ds = as.POSIXct(ds, tz = "UTC"),
        shrinkage = shr,
        hours = agents_with_shrinkage
      )
  }
}

hours_all <- bind_rows(hours_all)

monthly_2026 <- hours_all %>%
  mutate(
    month = as.Date(floor_date(ds, "month"))
  ) %>%
  filter(year(ds) == 2026) %>%
  group_by(shrinkage, month) %>%
  summarise(hours = sum(hours, na.rm = TRUE), .groups = "drop") %>%
  arrange(shrinkage, month)

out_csv <- file.path(paths$output, "Manning", "extra_analytics", "shrinkage_hours_2026_monthly.csv")
write_csv(monthly_2026, out_csv)

total_2026 <- monthly_2026 %>%
  group_by(shrinkage) %>%
  summarise(total_hours = sum(hours), .groups = "drop") %>%
  arrange(shrinkage)

out_total_csv <- file.path(paths$output, "Manning", "extra_analytics", "shrinkage_hours_2026_total.csv")
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

out_plot <- file.path(paths$output, "Manning", "extra_analytics", "shrinkage_hours_2026_monthly.png")
ggsave(out_plot, plot = p, width = 10, height = 5, dpi = 300)

walk(split(monthly_2026, monthly_2026$shrinkage), function(df_shr) {
  shr_val <- unique(df_shr$shrinkage)
  scenario_tag <- sprintf("shrinkage_%0.2f", shr_val)
  invisible(df_shr)
})

message("✔ Monthly table: ", out_csv)
message("✔ Total table: ", out_total_csv)
message("✔ Plot: ", out_plot)
