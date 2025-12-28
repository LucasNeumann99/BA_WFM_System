# ============================================================
# se_total.R
# ------------------------------------------------------------
# Helpers: SE total + split weights
# ============================================================

se_actual_share <- function(df_base,
                            cutoff_ds,
                            n_months = 6,
                            se1 = "Team SE 1, Travelcare",
                            se2 = "Team SE 2, Travelcare") {
  monthly <- df_base %>%
    filter(team %in% c(se1, se2), ds < cutoff_ds) %>%
    mutate(month = as.Date(lubridate::floor_date(ds, "month"))) %>%
    group_by(team, month) %>%
    summarise(calls = sum(y, na.rm = TRUE), .groups = "drop") %>%
    group_by(month) %>%
    mutate(
      total = sum(calls, na.rm = TRUE),
      share = if_else(total > 0, calls / total, NA_real_)
    ) %>%
    ungroup()
  
  if (nrow(monthly) == 0) {
    return(list(p_se1 = 0.5, p_se2 = 0.5))
  }
  
  last_month <- max(monthly$month, na.rm = TRUE)
  last_n <- sort(seq(last_month, by = "-1 month", length.out = n_months))
  
  rolling <- monthly %>%
    filter(month %in% last_n) %>%
    group_by(team) %>%
    summarise(share = mean(share, na.rm = TRUE), .groups = "drop")
  
  p_se1 <- rolling$share[rolling$team == se1]
  p_se2 <- rolling$share[rolling$team == se2]
  
  if (length(p_se1) == 0 || !is.finite(p_se1)) p_se1 <- 0.5
  if (length(p_se2) == 0 || !is.finite(p_se2)) p_se2 <- 1 - p_se1
  
  list(p_se1 = p_se1, p_se2 = p_se2)
}

ensure_se_share_cols <- function(fc_df,
                                 p_se1,
                                 p_se2,
                                 se1 = "Team SE 1, Travelcare",
                                 se2 = "Team SE 2, Travelcare",
                                 total = "Team SE total") {
  fc_df %>%
    mutate(
      p_se1 = if_else(team %in% c(se1, se2, total), p_se1, NA_real_),
      p_se2 = if_else(team %in% c(se1, se2, total), p_se2, NA_real_)
    )
}
