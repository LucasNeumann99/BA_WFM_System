# ============================================================
# 16_team_no_bias_plot.R
# ------------------------------------------------------------
# Formål:
# - Visualisere bias for Team NO før/efter justering
# - Understøtter narrativet om systematisk overforecast
# Output:
# - <output_base>/analysis_extra/Strukturelle ændringer/Team_no_customer_exit/
#   team_no_bias_before_after.png
# ============================================================

library(tidyverse)
library(here)

source(here("model_functions", "paths.R"))

paths <- get_pipeline_paths()
out_dir <- file.path(
  paths$output,
  "analysis_extra",
  "Strukturelle ændringer",
  "Team_no_customer_exit"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

in_path <- file.path(out_dir, "team_no_adjustment_comparison.csv")
if (!file.exists(in_path)) {
  stop("Mangler input: ", in_path)
}

df <- readr::read_csv(in_path, show_col_types = FALSE) %>%
  mutate(
    variant = factor(variant, levels = c("raw", "adjusted"))
  )

plot_colors <- c(
  "raw" = "#D93945",
  "adjusted" = "#3E3E3E"
)

p <- ggplot(df, aes(variant, Bias_mean, fill = variant)) +
  geom_col(width = 0.55) +
  geom_text(
    aes(label = sprintf("%.2f", Bias_mean)),
    vjust = -0.4,
    size = 4
  ) +
  scale_fill_manual(values = plot_colors, labels = c("Raw", "Justeret")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Team NO – systematisk overforecast før justering",
    subtitle = "Gns. bias (opkald/time). Raw ~ +2.5 viser overniveau efter kundens exit.",
    x = "",
    y = "Bias (opkald/time)",
    fill = ""
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

out_plot <- file.path(out_dir, "team_no_bias_before_after.png")
ggsave(out_plot, plot = p, width = 7, height = 4.5, dpi = 300)

message("✔ Plot gemt: ", out_plot)
