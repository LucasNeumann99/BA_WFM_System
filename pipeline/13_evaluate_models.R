# ============================================================
# 13_evaluate_models.R
# ------------------------------------------------------------
# Sammenligner:
#   - Baseline GLM (Poisson, NegBin)
#   - GLM + lags (Poisson_lags, NegBin_lags)
#
# Output:
#   - Forklaringsgrad (Explained R²)
#   - McFadden pseudo-R²
#   - Uplift i RMSE, MAPE, R² pr. team
#   - Plots til rapporten
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
baseline_metrics_path <- here("data_processed", "metrics_test_glm_by_team.rds")
lags_metrics_path     <- here("data_processed", "metrics_test_glm_lags_by_team.rds")

baseline_fc_path <- here("data_processed", "fc_test_glm_by_team.rds")
lags_fc_path     <- here("data_processed", "fc_test_glm_lags_by_team.rds")

models_base_path <- here("models", "baseline_glm_by_team.rds")
models_lags_path <- here("models", "glm_lags_by_team.rds")

out_metrics_path <- here("data_processed", "metrics_compare_baseline_vs_lags.rds")

plot_dir <- here("figures", "model_compare")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
metrics_base <- readRDS(baseline_metrics_path)
metrics_lags <- readRDS(lags_metrics_path)

fc_base <- readRDS(baseline_fc_path)
fc_lags <- readRDS(lags_fc_path)

models_base <- readRDS(models_base_path)
models_lags <- readRDS(models_lags_path)

# ============================================================
# 1. Explained Variance R²
# ============================================================

compute_explained_variance <- function(y, yhat) {
  1 - var(y - yhat, na.rm = TRUE) / var(y, na.rm = TRUE)
}

# --- Baseline (Poisson + NegBin) ---
r2_base <- fc_base %>%
  pivot_longer(
    cols = c(y_pois, y_nb),
    names_to = "model",
    values_to = "yhat"
  ) %>%
  group_by(team, model) %>%
  summarise(R2_explained = compute_explained_variance(y, yhat),
            .groups = "drop") %>%
  mutate(model = recode(
    model,
    "y_pois" = "GLM_Poisson",
    "y_nb"   = "GLM_NegBin"
  ))

# --- Lags (Poisson_lags + NegBin_lags) ---
r2_lags <- fc_lags %>%
  pivot_longer(
    cols = c(y_pois_lags, y_nb_lags),
    names_to = "model",
    values_to = "yhat"
  ) %>%
  group_by(team, model) %>%
  summarise(R2_explained = compute_explained_variance(y, yhat),
            .groups = "drop") %>%
  mutate(model = recode(
    model,
    "y_pois_lags" = "GLM_Poisson_lags",
    "y_nb_lags"   = "GLM_NegBin_lags"
  ))

# ============================================================
# 2. McFadden pseudo-R² (kræver modelobjekter)
# ============================================================

compute_mcfadden <- function(model) {
  # Brug deviance/null.deviance for at undgå re-fit med manglende data i scope
  if (is.null(model)) return(NA_real_)
  if (is.null(model$null.deviance) || model$null.deviance == 0) return(NA_real_)
  1 - (model$deviance / model$null.deviance)
}

# --- Baseline ---
mcf_base <- tibble(
  team  = rep(names(models_base), each = 2),
  model = rep(c("GLM_Poisson", "GLM_NegBin"), length(models_base)),
  R2_mcfadden = unlist(lapply(models_base, function(m) {
    list(
      compute_mcfadden(m$poisson),
      compute_mcfadden(m$negbin)
    )
  }))
)

# --- Lags ---
mcf_lags <- tibble(
  team  = rep(names(models_lags), each = 2),
  model = rep(c("GLM_Poisson_lags", "GLM_NegBin_lags"), length(models_lags)),
  R2_mcfadden = unlist(lapply(models_lags, function(m) {
    list(
      compute_mcfadden(m$poisson_lags),
      compute_mcfadden(m$negbin_lags)
    )
  }))
)

# ============================================================
# 3. Læg R² på metrics (baseline og lags hver for sig)
# ============================================================

metrics_base2 <- metrics_base %>%
  left_join(r2_base, by = c("team", "model")) %>%
  left_join(mcf_base, by = c("team", "model"))

metrics_lags2 <- metrics_lags %>%
  left_join(r2_lags, by = c("team", "model")) %>%
  left_join(mcf_lags, by = c("team", "model"))

# ============================================================
# 4. Vælg bedste model pr. team (lavest RMSE)
#    - én baseline-model pr. team
#    - én lags-model pr. team
# ============================================================

best_base <- metrics_base2 %>%
  group_by(team) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>%
  ungroup()

best_lags <- metrics_lags2 %>%
  group_by(team) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>%
  ungroup()

# ============================================================
# 5. Byg "smart matrix": én række pr. team
# ============================================================

compare <- best_base %>%
  transmute(
    team,
    model_baseline = model,
    n_baseline     = n,
    RMSE_baseline  = RMSE,
    MAE_baseline   = MAE,
    MAPE_baseline  = MAPE,
    SMAPE_baseline = SMAPE,
    Bias_mean_baseline = Bias_mean,
    Bias_sd_baseline   = Bias_sd,
    R2_explained_baseline = R2_explained,
    R2_mcfadden_baseline  = R2_mcfadden
  ) %>%
  left_join(
    best_lags %>%
      transmute(
        team,
        model_lags = model,
        n_lags     = n,
        RMSE_lags  = RMSE,
        MAE_lags   = MAE,
        MAPE_lags  = MAPE,
        SMAPE_lags = SMAPE,
        Bias_mean_lags = Bias_mean,
        Bias_sd_lags   = Bias_sd,
        R2_explained_lags = R2_explained,
        R2_mcfadden_lags  = R2_mcfadden
      ),
    by = "team"
  ) %>%
  mutate(
    RMSE_uplift = (RMSE_baseline - RMSE_lags) / RMSE_baseline,
    MAPE_uplift = (MAPE_baseline - MAPE_lags) / MAPE_baseline,
    R2_gain     = R2_explained_lags - R2_explained_baseline
  )

print(compare)

# Gem matrix til videre brug (rapport / tabeller)
saveRDS(compare, out_metrics_path)
message("✔ Metrics samlet og gemt i: ", out_metrics_path)

# ============================================================
# 6. Plots (RMSE uplift + R² gain)
# ============================================================

# --- RMSE uplift ---
p_rmse <- ggplot(compare, aes(x = team, y = RMSE_uplift)) +
  geom_col(fill = "#D93945") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "RMSE Uplift (GLM+lags vs Baseline — bedste model pr. team)",
    y     = "Uplift (%)",
    x     = ""
  )

ggsave(file.path(plot_dir, "rmse_uplift.png"),
       p_rmse, width = 8, height = 4)

# --- R² gain ---
p_r2 <- ggplot(compare, aes(x = team, y = R2_gain)) +
  geom_col(fill = "#2E9A5D") +
  coord_flip() +
  labs(
    title = "Explained Variance R² — Gain (GLM+lags vs Baseline)",
    y     = "Δ R²",
    x     = ""
  )

ggsave(file.path(plot_dir, "r2_gain.png"),
       p_r2, width = 8, height = 4)

message("✔ Plots gemt i: ", plot_dir)
