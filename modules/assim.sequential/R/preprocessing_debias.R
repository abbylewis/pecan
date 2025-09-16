#' Debias preprocessing utilities (helpers for SDA debias step)
#'
#' These functions encapsulate the small-but-fiddly preprocessing bits used by
#' the debias step in sda.enkf.multisite(): mapping obs to columns, collecting
#' covariates for the correct year, and organizing comparison/diagnostic tables.
#'
#' All functions are pure (no hidden state) and accept `settings` explicitly.
#' Keep `settings$covariates_df` and `settings$site_coords` populated upstream.
#'
#' @author Shashank Ramachandran
#' @noRd

# ---- Name mapping (edit here if variable names differ between OBS and STATE) ----
debias_name_map <- c(
  AGB   = "AbvGrndWood",
  LAI   = "LAI",
  SMP   = "SoilMoistFrac",
  SoilC = "TotSoilCarb"
)

# ---- Covariate accessor for a given observation datetime ----
debias_get_covariates_for_date <- function(settings, obs_date) {
  yr <- lubridate::year(obs_date)
  
  if (is.null(settings$covariates_df)) {
    stop("settings$covariates_df is NULL. Supply covariate table with columns: site, year, <features...>.")
  }
  if (is.null(settings$site_coords)) {
    stop("settings$site_coords is NULL. Supply a data.frame with a 'site' column listing site IDs in use.")
  }
  
  settings$covariates_df %>%
    dplyr::filter(.data$year == yr) %>%
    dplyr::right_join(dplyr::select(settings$site_coords, .data$site), by = "site") %>%
    dplyr::arrange(.data$site)
}

# ---- Expand covariates to match columns of X (site-wise repetition) ----
debias_cov_by_columns <- function(settings, obs_date, site_index) {
  df <- debias_get_covariates_for_date(settings, obs_date)
  idx <- match(site_index, df$site)  # repeats each site's row per (site,var) column
  as.matrix(df[idx, setdiff(names(df), c("site","year")), drop = FALSE])
}

# ---- Build an observation vector aligned to X's columns ----
# col_vars: character vector of state-variable names, length ncol(X)
# site_index: attribute vector (same length as col_vars) labeling columns by site
debias_obs_vec_for_time <- function(t_idx, site_index, col_vars, obs.mean, name_map = debias_name_map) {
  om  <- obs.mean[[t_idx]]
  out <- rep(NA_real_, length(col_vars))
  
  # Allow mapping OBS variable names -> STATE variable names
  for (s in unique(site_index)) {
    vals <- om[[as.character(s)]]
    if (is.null(vals)) next
    
    if (!is.null(name_map)) {
      keep <- names(vals) %in% names(name_map)
      if (any(keep)) names(vals)[keep] <- unname(name_map[names(vals)[keep]])
    }
    
    v_here <- unique(col_vars[site_index == s])
    vnames <- intersect(names(vals), v_here)
    for (v in vnames) {
      idx <- which(site_index == s & col_vars == v)
      if (length(idx)) out[idx] <- as.numeric(vals[[v]][1])
    }
  }
  out
}

# ---- Convenience: build tidy comparison table for diagnostics ----
debias_build_comp_df <- function(site_index, col_vars, pre_mean, post_mean, obs_vec) {
  df <- data.frame(
    site = site_index,
    var  = col_vars,
    pre  = as.numeric(pre_mean),
    post = as.numeric(post_mean),
    obs  = as.numeric(obs_vec),
    stringsAsFactors = FALSE
  )
  df[order(df$var, df$site), ]
}

# ---- Small RMSE summary by variable ----
debias_rmse_by_var <- function(comp_df) {
  rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
  do.call(
    rbind,
    lapply(split(comp_df, comp_df$var), function(d) {
      data.frame(
        var       = d$var[1],
        rmse_pre  = rmse(d$pre,  d$obs),
        rmse_post = rmse(d$post, d$obs),
        stringsAsFactors = FALSE
      )
    })
  )
}


