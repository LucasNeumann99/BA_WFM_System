# ============================================================
# se_total.R
# ------------------------------------------------------------
# Helper: build SE total + split weights
# ============================================================

build_se_total <- function(fc_df,
                           se1 = "Team SE 1, Travelcare",
                           se2 = "Team SE 2, Travelcare",
                           total = "Team SE total") {
  yhat_col <- if ("y_hat_raw" %in% names(fc_df)) "y_hat_raw" else if ("y_hat" %in% names(fc_df)) "y_hat" else NULL
  if (is.null(yhat_col)) {
    stop("build_se_total: mangler y_hat_raw/y_hat i input.")
  }
  
  se_df <- fc_df %>% filter(team %in% c(se1, se2))
  if (nrow(se_df) == 0) {
    return(fc_df)
  }
  
  agg <- se_df %>%
    group_by(ds) %>%
    summarise(
      y_hat_total = sum(.data[[yhat_col]], na.rm = TRUE),
      y_total = if ("y" %in% names(se_df)) sum(y, na.rm = TRUE) else NA_real_,
      y_hat_se1 = sum(.data[[yhat_col]][team == se1], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      p_se1 = if_else(y_hat_total > 0, y_hat_se1 / y_hat_total, 0.5),
      p_se2 = 1 - p_se1
    )
  
  total_rows <- agg %>%
    transmute(
      team = total,
      ds = ds,
      y_hat_total = y_hat_total,
      y_total = y_total,
      p_se1 = p_se1,
      p_se2 = p_se2
    )
  
  total_rows[[yhat_col]] <- total_rows$y_hat_total
  if ("model_used" %in% names(fc_df)) {
    total_rows$model_used <- "AGG_SE1_SE2"
  }
  if ("is_recursive" %in% names(fc_df)) {
    total_rows$is_recursive <- FALSE
  }
  
  se_with_p <- se_df %>%
    left_join(agg %>% dplyr::select(ds, p_se1, p_se2), by = "ds")
  
  other <- fc_df %>% filter(!team %in% c(se1, se2))
  
  out <- bind_rows(other, se_with_p, total_rows)
  out
}
