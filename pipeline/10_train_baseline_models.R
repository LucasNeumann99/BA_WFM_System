# ============================================================
# 10_train_baseline_models.R
# ------------------------------------------------------------
# Formål:
# - Træne simple, forklarbare baseline-modeller (Poisson & NegBin)
#   pr. team til hourly call volume
# - Train: 2022-01-01 → 2024-12-31
# - Test:  2025-01-01 → 2025-11-11 (eller sidste tilgængelige dato)
# - Kun kalender- og tidsfeatures (ingen lags)
# - Gemme modeller + test-forecast + metrics
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(MASS)   # glm.nb til Negative Binomial

select <- dplyr::select
filter <- dplyr::filter
lag    <- dplyr::lag

# ---- paths ----
in_path        <- here("data_processed", "ts_hourly_all_teams_struct_adj.rds")
models_out_path  <- here("models", "baseline_glm_by_team.rds")
fc_test_out_path <- here("data_processed", "fc_test_glm_by_team.rds")
metrics_out_path <- here("data_processed", "metrics_test_glm_by_team.rds")

# sikre at models-mappe findes
if (!dir.exists(here("models"))) {
  dir.create(here("models"), recursive = TRUE)
}

# ---- load historical data ----
ts_hist <- readRDS(in_path)

# sanity
stopifnot(all(c("team", "ds", "y") %in% names(ts_hist)))
stopifnot(inherits(ts_hist$ds, "POSIXct"))

# ---- train / test cutoffs ----
train_end_date <- ymd("2024-12-31")
test_start_dt  <- ymd_hms("2025-01-01 00:00:00", tz = "UTC")
test_end_date  <- ymd("2025-11-11")  # juster hvis du vil

# reel test-slut = min(test_end_date, sidste timestamp i data)
max_ds <- max(ts_hist$ds)
test_end_dt <- min(
  ymd_hms(paste0(test_end_date, " 23:00:00"), tz = "UTC"),
  max_ds
)

message("Train: <  ", train_end_date)
message("Test:  >= ", test_start_dt, "  &  <= ", test_end_dt)

# ---- featurevalg (kun kalender/tid/ferie) ----
# juster her, hvis du vil tilføje/fjerne noget
calendar_features <- c(
  "hour",
  "weekday",    
  "week",
  "month",
  "year",
  "Vinterferie",
  "Påskeferie",
  "Sommerferie",
  "Efterårsferie",
  "Juleferie"
)

missing_feats <- setdiff(calendar_features, names(ts_hist))
if (length(missing_feats) > 0) {
  stop("Følgende forventede features mangler i ts_hist: ",
       paste(missing_feats, collapse = ", "))
}

# ---- helper: metrics ----
compute_metrics <- function(df, y_col, yhat_col, model_name, team_name) {
  y     <- df[[y_col]]
  y_hat <- df[[yhat_col]]
  
  residual <- y - y_hat
  
  tibble(
    team      = team_name,
    model     = model_name,
    n         = sum(!is.na(y) & !is.na(y_hat)),
    RMSE      = sqrt(mean((y - y_hat)^2, na.rm = TRUE)),
    MAE       = mean(abs(y - y_hat), na.rm = TRUE),
    MAPE      = mean(ifelse(y == 0, NA_real_, abs((y - y_hat) / y)), na.rm = TRUE),
    SMAPE     = mean(
      2 * abs(y - y_hat) / (abs(y) + abs(y_hat)),
      na.rm = TRUE
    ),
    Bias_mean = mean(residual, na.rm = TRUE),
    Bias_sd   = sd(residual, na.rm = TRUE)
  )
}

# ---- split per team & train modeller ----
teams <- sort(unique(ts_hist$team))

models_by_team <- list()
all_fc_test    <- list()
all_metrics    <- list()

for (tm in teams) {
  message("------ Team: ", tm, " ------")
  
  df_team <- ts_hist %>%
    filter(team == tm) %>%
    arrange(ds)
  
  # split
  train_df <- df_team %>%
    filter(ds <  train_end_date + days(1))   # inkl. hele 2024
  test_df  <- df_team %>%
    filter(ds >= test_start_dt,
           ds <= test_end_dt)
  
  # vælg features + target
  train_mod <- train_df %>%
    select(y, all_of(calendar_features)) %>%
    drop_na()   # burde ikke være NA, men for en sikkerheds skyld
  
  test_mod <- test_df %>%
    select(y, all_of(calendar_features)) %>%
    drop_na()
  
  if (nrow(train_mod) == 0 || nrow(test_mod) == 0) {
    warning("Team ", tm, " har ikke nok data til træning/test – springes over.")
    next
  }
  
  # formula – brug faktorer hvor det giver mening
  # (weekday er allerede faktor fra trin 06)
  fml <- as.formula(
    paste(
      "y ~",
      paste(
        c(
          "factor(hour)",
          "weekday",
          "factor(week)",
          "factor(month)",
          "year",
          "Vinterferie",
          "Påskeferie",
          "Sommerferie",
          "Efterårsferie",
          "Juleferie"
        ),
        collapse = " + "
      )
    )
  )
  
  # ---- Poisson GLM ----
  message("  · Træner Poisson GLM")
  mod_pois <- glm(
    formula = fml,
    family  = poisson(link = "log"),
    data    = train_mod
  )
  
  # ---- Negativ Binomial GLM ----
  message("  · Træner Negative Binomial GLM")
  mod_nb <- tryCatch(
    MASS::glm.nb(formula = fml, data = train_mod),
    error = function(e) {
      warning("NegBin konvergerede ikke for team ", tm, 
              " – bruger kun Poisson. Fejl: ", e$message)
      NULL
    }
  )
  
  # ---- predict på test ----
  test_pred <- test_df %>%
    mutate(
      y_pois = predict(mod_pois,
                       newdata = .,
                       type = "response"),
      y_nb   = if (!is.null(mod_nb)) {
        predict(mod_nb, newdata = ., type = "response")
      } else {
        NA_real_
      }
    )
  
  # ---- metrics ----
  metrics_pois <- compute_metrics(
    df        = test_pred,
    y_col     = "y",
    yhat_col  = "y_pois",
    model_name = "GLM_Poisson",
    team_name  = tm
  )
  
  metrics_list <- list(metrics_pois)
  
  if (!is.null(mod_nb)) {
    metrics_nb <- compute_metrics(
      df        = test_pred,
      y_col     = "y",
      yhat_col  = "y_nb",
      model_name = "GLM_NegBin",
      team_name  = tm
    )
    metrics_list[[length(metrics_list) + 1]] <- metrics_nb
  }
  
  # ---- gem i samleobjekter ----
  models_by_team[[tm]] <- list(
    poisson = mod_pois,
    negbin  = mod_nb
  )
  
  all_fc_test[[tm]] <- test_pred %>%
    mutate(team = tm) %>%
    select(team, ds, y, y_pois, y_nb)
  
  all_metrics[[tm]] <- bind_rows(metrics_list)
}

# ---- bind & save ----
fc_test_all    <- bind_rows(all_fc_test)
metrics_all    <- bind_rows(all_metrics)

saveRDS(models_by_team, models_out_path)
saveRDS(fc_test_all,    fc_test_out_path)
saveRDS(metrics_all,    metrics_out_path)

message("✔ Baseline GLM-modeller trænet for alle teams")
message("✔ Test-forecast gemt: ", fc_test_out_path)
message("✔ Metrics gemt:       ", metrics_out_path)
message("✔ Model-objekter gemt: ", models_out_path)
