# ============================================================
# 12_se_routing_split_key.R
# ------------------------------------------------------------
# Formål:
# - Plot fordelingsnøgle p_SE1,t over tid (uge/måned)
# - Bruges til Team SE routing problem analyse
# Output:
# - <output_base>/analysis_extra/Strukturelle ændringer/Team_se_routing_problem/
#   se_routing_split_key_over_time.png
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(
  paths$output,
  "analysis_extra",
  "Strukturelle ændringer",
  "Team_se_routing_problem"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fc_path <- file.path(paths$results, "v2", "operational", "fc_operational_raw_v2.rds")
fc <- readRDS(fc_path)

se <- fc %>%
  filter(team %in% c("Team SE 1, Travelcare", "Team SE 2, Travelcare")) %>%
  select(team, ds, y_hat_raw) %>%
  pivot_wider(names_from = team, values_from = y_hat_raw)

se <- se %>%
  mutate(
    total = `Team SE 1, Travelcare` + `Team SE 2, Travelcare`,
    p_se1 = if_else(total > 0, `Team SE 1, Travelcare` / total, NA_real_)
  )

plot_df <- se %>%
  mutate(month = as.Date(floor_date(ds, "month"))) %>%
  group_by(month) %>%
  summarise(p_se1 = mean(p_se1, na.rm = TRUE), .groups = "drop")

p <- ggplot(plot_df, aes(month, p_se1)) +
  geom_line(color = "#D93945", linewidth = 1.1) +
  geom_smooth(method = "loess", se = FALSE, color = "#3E3E3E", linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Fordelingsnøgle over tid (SE1 af total)",
    subtitle = "p_SE1,t = SE1 / (SE1 + SE2), månedligt gennemsnit",
    x = "Måned",
    y = "p_SE1,t"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

out_path <- file.path(out_dir, "se_routing_split_key_over_time.png")
ggsave(out_path, plot = p, width = 10, height = 5, dpi = 300)

message("✔ Plot gemt: ", out_path)
