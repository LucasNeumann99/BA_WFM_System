# ============================================================
# 11_evaluate_test_forecasts.R
# ------------------------------------------------------------
# Evaluerer baseline GLM (Poisson & NegBin) på testperioden
# + Tilføjer Explained R² og McFadden pseudo-R²
# + Gemmer alle plots i test_evaluation/<team> (repo-root)
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(viridis)

source(here("model_functions", "paths.R"))
paths <- get_pipeline_paths()

# ---- paths ----
fc_path      <- here("data_processed", "fc_test_glm_by_team.rds")
metrics_path <- here("data_processed", "metrics_test_glm_by_team.rds")
models_base_path <- here("models", "baseline_glm_by_team.rds")

# ---- palette ----
plot_colors <- c(
  "Actual" = "#3E3E3E",  # Gray_dark
  "Poisson" = "#2A6F97", # Blue
  "NegBin" = "#D93945"   # SOS_red
)

fig_base_dir <- here("test_evaluation")
dir.create(fig_base_dir, recursive = TRUE, showWarnings = FALSE)

# ---- load data ----
fc_test   <- readRDS(fc_path)
metrics   <- readRDS(metrics_path)
models_base <- readRDS(models_base_path)

fc_test <- fc_test %>%
  mutate(
    ds   = as.POSIXct(ds, tz = "UTC"),
    team = as.character(team)
  )

# ============================================================
# 1) R² + McFadden pseudo-R²
# ============================================================

compute_explained_variance <- function(y, yhat) {
  1 - var(y - yhat, na.rm = TRUE) / var(y, na.rm = TRUE)
}

compute_mcfadden <- function(model) {
  # Brug deviance/null.deviance direkte for at undgå at re-fit med data, der ikke er i scope
  if (is.null(model)) return(NA_real_)
  if (is.null(model$null.deviance) || model$null.deviance == 0) return(NA_real_)
  1 - (model$deviance / model$null.deviance)
}

# ---- Explained R² ----
r2_explained <- fc_test %>%
  pivot_longer(cols = c(y_pois, y_nb),
               names_to = "model",
               values_to = "yhat") %>%
  group_by(team, model) %>%
  summarise(
    R2_explained = compute_explained_variance(y, yhat),
    .groups = "drop"
  ) %>%
  mutate(
    model = recode(model,
                   "y_pois" = "GLM_Poisson",
                   "y_nb"   = "GLM_NegBin")
  )

# ---- McFadden pseudo-R² ----
mcfadden_r2 <- tibble(
  team  = rep(names(models_base), each = 2),
  model = rep(c("GLM_Poisson", "GLM_NegBin"), times = length(models_base)),
  R2_mcfadden = unlist(lapply(models_base, function(m) {
    list(
      compute_mcfadden(m$poisson),
      compute_mcfadden(m$negbin)
    )
  }))
)

# ---- Combine metrics ----
metrics_final <- metrics %>%
  left_join(r2_explained, by = c("team", "model")) %>%
  left_join(mcfadden_r2, by = c("team", "model"))

# ---- Save updated metrics ----
perf_dir <- file.path(fig_base_dir, "tables")
dir.create(perf_dir, recursive = TRUE, showWarnings = FALSE)

perf_table_path <- file.path(perf_dir, "metrics_test_glm_by_team.csv")
write_csv(metrics_final, perf_table_path)

message("✔ Performance tabel gemt: ", perf_table_path)

# ============================================================
# 2) Plot helpers
# ============================================================

plot_forecast_vs_actual <- function(df_team, team_name) {
  df_long <- df_team %>%
    pivot_longer(
      cols      = c(y, y_pois, y_nb),
      names_to  = "series",
      values_to = "value"
    ) %>%
    mutate(
      series = factor(series,
                      levels = c("y", "y_pois", "y_nb"),
                      labels = c("Actual", "Poisson", "NegBin"))
    )
  
  ggplot(df_long, aes(ds, value, colour = series)) +
    geom_line() +
    scale_colour_manual(values = plot_colors, breaks = names(plot_colors)) +
    theme_minimal() +
    labs(title = paste("Forecast vs Actual –", team_name),
         x = "Tid", y = "Antal opkald")
}

plot_forecast_week_zoom <- function(df_team, team_name) {
  start_week <- floor_date(min(df_team$ds), "week")
  end_week   <- start_week + days(7)
  
  df_zoom <- df_team %>% filter(ds >= start_week, ds < end_week)
  
  df_long <- df_zoom %>%
    pivot_longer(c(y, y_pois, y_nb),
                 names_to = "series", values_to = "value") %>%
    mutate(series = factor(series,
                           levels = c("y", "y_pois", "y_nb"),
                           labels = c("Actual", "Poisson", "NegBin")))
  
  ggplot(df_long, aes(ds, value, colour = series)) +
    geom_line() +
    scale_colour_manual(values = plot_colors, breaks = names(plot_colors)) +
    theme_minimal() +
    labs(title = paste("Forecast vs Actual – uge-zoom (", team_name, ")"),
         x = "Tid", y = "Antal opkald")
}

plot_residuals_time <- function(df_team, team_name) {
  df_res <- df_team %>%
    mutate(
      resid_pois = y - y_pois,
      resid_nb   = y - y_nb
    ) %>%
    pivot_longer(c(resid_pois, resid_nb),
                 names_to = "model", values_to = "residual") %>%
    mutate(model = recode(model,
                          resid_pois = "Poisson",
                          resid_nb   = "NegBin"))
  
  ggplot(df_res, aes(ds, residual, colour = model)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    scale_colour_manual(values = c("Poisson" = "#2A6F97", "NegBin" = "#D93945")) +
    theme_minimal() +
    labs(title = paste("Residualer over tid –", team_name),
         x = "Tid", y = "Residual")
}

plot_residual_hist <- function(df_team, team_name) {
  df_res <- df_team %>%
    mutate(
      resid_pois = y - y_pois,
      resid_nb   = y - y_nb
    ) %>%
    pivot_longer(c(resid_pois, resid_nb),
                 names_to = "model", values_to = "residual") %>%
    mutate(model = recode(model,
                          resid_pois = "Poisson",
                          resid_nb   = "NegBin"))
  
  ggplot(df_res, aes(residual)) +
    geom_histogram(bins = 50) +
    facet_wrap(~ model, ncol = 1, scales = "free_y") +
    theme_minimal() +
    labs(title = paste("Residualfordeling –", team_name),
         x = "Residual", y = "Frekvens")
}

plot_residual_acf <- function(df_team, team_name, best_model) {
  res <- if (best_model == "GLM_NegBin") df_team$y - df_team$y_nb
  else                           df_team$y - df_team$y_pois
  
  res <- res[is.finite(res)]
  
  if (length(res) < 10) return(NULL)
  
  ac <- acf(res, plot = FALSE, lag.max = 60)
  df_acf <- tibble(lag = ac$lag[,1,1], acf = ac$acf[,1,1])
  
  ggplot(df_acf, aes(lag, acf)) +
    geom_segment(aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    labs(title = paste("ACF residualer –", team_name),
         x = "Lag", y = "ACF")
}

plot_error_heatmap <- function(df_team, team_name, best_model) {
  df_err <- df_team %>%
    mutate(
      hour    = hour(ds),
      weekday = wday(ds, label = TRUE, week_start = 1),
      
      # FIX: brug if() i stedet for if_else()
      yhat = if (best_model == "GLM_NegBin") y_nb else y_pois,
      
      ape = abs(y - yhat) / pmax(y, 1)
    )
  
  agg <- df_err %>%
    group_by(weekday, hour) %>%
    summarise(MAPE = mean(ape), .groups = "drop")
  
  ggplot(agg, aes(hour, weekday, fill = MAPE)) +
    geom_tile() +
    scale_fill_gradient(low = "#3E3E3E", high = "#D93945") +
    theme_minimal() +
    labs(
      title = paste("MAPE heatmap –", best_model, team_name),
      x = "Time på dagen",
      y = "Ugedag"
    )
}

# ============================================================
# 3) Best model per team (SMAPE)
# ============================================================

best_by_team <- metrics %>%
  group_by(team) %>%
  slice_min(SMAPE, n = 1) %>%
  ungroup() %>%
  select(team, best_model = model)

# ============================================================
# 4) Generate plots per team
# ============================================================

teams <- unique(fc_test$team)

for (tm in teams) {
  
  message("---- Plotter for: ", tm)
  
  df_team <- fc_test %>% filter(team == tm)
  best_model <- best_by_team %>% filter(team == tm) %>% pull(best_model)
  
  tdir <- file.path(fig_base_dir, tm)
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  
  ggsave(file.path(tdir, "forecast_vs_actual_full.png"),
         plot_forecast_vs_actual(df_team, tm), width = 10, height = 5)
  
  ggsave(file.path(tdir, "forecast_vs_actual_week_zoom.png"),
         plot_forecast_week_zoom(df_team, tm), width = 10, height = 5)
  
  ggsave(file.path(tdir, "residuals_time.png"),
         plot_residuals_time(df_team, tm), width = 10, height = 5)
  
  ggsave(file.path(tdir, "residuals_hist.png"),
         plot_residual_hist(df_team, tm), width = 8, height = 6)
  
  p_acf <- plot_residual_acf(df_team, tm, best_model)
  if (!is.null(p_acf))
    ggsave(file.path(tdir, "residuals_acf_best_model.png"),
           p_acf, width = 8, height = 5)
  
  ggsave(file.path(tdir, "error_heatmap_best_model.png"),
         plot_error_heatmap(df_team, tm, best_model), width = 8, height = 5)
}

message("✔ Test-evaluering fuldført")
