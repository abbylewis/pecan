#' Debias preprocessing utilities (internal)
#'
#' Helper functions for the SDA debias step used by `sda.enkf.multisite()`:
#'
#' 1. Name mapping between observation variable names and state variable names.
#' 2. Site filtering (toggle-able):
#'    - drop sites with missing covariates in the current year;
#'    - drop sites that became *inconsistent* in observations over time
#'      (e.g., AGB present in 2012 but missing in 2013).
#' 3. Covariate extraction for the current year and alignment to X’s column layout.
#' 4. Observation vector builder aligned to X’s columns.
#' 5. Diagnostics (pre/post comparison and RMSE by variable).
#'
#' These helpers are pure (stateless) and do not use `settings`.
#'
#' @keywords internal
#' @name debias_helpers
#' @noRd
NULL

# ------------------------------------------------------------------------------
# (1) Name mapping
# ------------------------------------------------------------------------------

#' @rdname debias_helpers
#' @keywords internal
#' Map OBS names -> STATE names (edit if your naming changes).
debias_name_map <- c(
  AGB   = "AbvGrndWood",
  LAI   = "LAI",
  SMP   = "SoilMoistFrac",
  SoilC = "TotSoilCarb"
)

# ------------------------------------------------------------------------------
# (2) Site filtering utilities
# ------------------------------------------------------------------------------

#' @rdname debias_helpers
#' @keywords internal
#' Determine sites that have complete (non-NA) covariates in a given year.
#'
#' @param covariates_df long table with columns: site, year, <covariate layers...>
#' @param year integer year to check
#' @param candidate_sites character vector of site ids to consider
#' @return character vector of sites that have no NA in covariate columns for that year
debias_sites_with_complete_covariates_year <- function(covariates_df, year, candidate_sites) {
  df_year <- covariates_df[
    covariates_df$year == as.integer(year) & covariates_df$site %in% candidate_sites,
    , drop = FALSE
  ]
  if (nrow(df_year) == 0) return(character(0))
  
  cov_cols <- setdiff(names(df_year), c("site", "year"))
  if (length(cov_cols) == 0) return(character(0))
  
  ok_mask <- rowSums(is.na(df_year[, cov_cols, drop = FALSE])) == 0L
  df_year$site[ok_mask]
}

#' @rdname debias_helpers
#' @keywords internal
#' Identify sites that became inconsistent in observed variables across time.
#'
#' A site is inconsistent at t_idx if any variable that was ever observed for that
#' site in earlier times (1..t_idx-1) is missing at time t_idx.
debias_sites_inconsistent_obs <- function(obs.mean, t_idx, name_map = debias_name_map) {
  if (t_idx <= 1L) return(character(0))
  
  observed_vars_at <- function(tt, site_id) {
    om <- obs.mean[[tt]][[as.character(site_id)]]
    if (is.null(om)) return(character(0))
    vn <- names(om)
    if (!is.null(name_map)) {
      keep <- vn %in% names(name_map)
      if (any(keep)) vn[keep] <- unname(name_map[vn[keep]])
    }
    vn
  }
  
  all_sites <- unique(unlist(lapply(obs.mean[seq_len(t_idx)], function(om_t) names(om_t))), use.names = FALSE)
  all_sites <- as.character(all_sites)
  
  inconsistent <- character(0)
  for (s in all_sites) {
    prev_union <- unique(unlist(lapply(seq_len(t_idx - 1L), observed_vars_at, site_id = s), use.names = FALSE))
    if (length(prev_union) == 0) next
    cur_vars <- observed_vars_at(t_idx, s)
    if (length(setdiff(prev_union, cur_vars)) > 0) {
      inconsistent <- c(inconsistent, s)
    }
  }
  unique(inconsistent)
}

# ------------------------------------------------------------------------------
# (3) Covariate accessors aligned to time and X’s layout
# ------------------------------------------------------------------------------

#' @rdname debias_helpers
#' @keywords internal
#' Fetch covariates for the current year and filter sites:
#'   - optionally drop sites with missing covariates this year;
#'   - optionally drop sites inconsistent in observations up to `t_idx`.
debias_get_covariates_for_date <- function(covariates_df,
                                           obs_date,
                                           site_index,
                                           obs.mean,
                                           t_idx,
                                           drop_incomplete_covariates = TRUE,
                                           enforce_consistent_obs = TRUE) {
  if (is.null(covariates_df)) {
    stop("covariates_df is NULL. Provide columns: site, year, <features...>.")
  }
  yr <- lubridate::year(obs_date)
  sites_used <- unique(as.character(site_index))
  
  # 1) filter by complete covariates this year (optional)
  if (isTRUE(drop_incomplete_covariates)) {
    complete_sites <- debias_sites_with_complete_covariates_year(covariates_df, yr, sites_used)
  } else {
    complete_sites <- intersect(
      sites_used,
      as.character(covariates_df$site[covariates_df$year == as.integer(yr)])
    )
  }
  
  # 2) optionally filter out sites with inconsistent obs across time
  if (isTRUE(enforce_consistent_obs)) {
    if (is.null(obs.mean) || is.null(t_idx)) {
      stop("obs.mean and t_idx must be provided when enforce_consistent_obs = TRUE.")
    }
    inconsistent_sites <- debias_sites_inconsistent_obs(obs.mean, t_idx, name_map = debias_name_map)
    eligible_sites <- setdiff(complete_sites, inconsistent_sites)
  } else {
    eligible_sites <- complete_sites
  }
  
  if (length(eligible_sites) == 0) {
    return(tibble::tibble(site = character(0), year = integer(0)))
  }
  
  df_year <- covariates_df[
    covariates_df$year == as.integer(yr) & covariates_df$site %in% eligible_sites,
    , drop = FALSE
  ]
  df_year <- df_year[order(df_year$site), , drop = FALSE]
  
  # annotate what we dropped (useful for logging)
  attr(df_year, "dropped_missing_covariates") <- setdiff(sites_used, complete_sites)
  if (isTRUE(enforce_consistent_obs)) {
    attr(df_year, "dropped_inconsistent_obs") <- intersect(sites_used, debias_sites_inconsistent_obs(obs.mean, t_idx))
  }
  
  df_year
}

#' @rdname debias_helpers
#' @keywords internal
#' Expand per-site covariates into a row-per-column matrix aligned with X’s layout.
#' For columns whose site was filtered out, emit a row of NA features (shape-preserving).
debias_cov_by_columns <- function(covariates_df,
                                  obs_date,
                                  site_index,
                                  obs.mean,
                                  t_idx,
                                  drop_incomplete_covariates = TRUE,
                                  enforce_consistent_obs = TRUE) {
  df_year <- debias_get_covariates_for_date(
    covariates_df = covariates_df,
    obs_date      = obs_date,
    site_index    = site_index,
    obs.mean      = obs.mean,
    t_idx         = t_idx,
    drop_incomplete_covariates = drop_incomplete_covariates,
    enforce_consistent_obs     = enforce_consistent_obs
  )
  
  if (nrow(df_year) == 0) {
    return(matrix(numeric(0), nrow = length(site_index), ncol = 0))
  }
  
  feat_cols <- setdiff(names(df_year), c("site", "year"))
  idx <- match(as.character(site_index), df_year$site)
  
  na_row <- as.list(rep(NA_real_, length(feat_cols)))
  names(na_row) <- feat_cols
  filler <- tibble::as_tibble_row(na_row)
  
  rows <- lapply(seq_along(idx), function(i) {
    j <- idx[i]
    if (is.na(j)) filler else df_year[j, feat_cols, drop = FALSE]
  })
  out <- dplyr::bind_rows(rows)
  
  as.matrix(out)
}

# ------------------------------------------------------------------------------
# (4) Observation vector aligned to X’s columns
# ------------------------------------------------------------------------------

#' @rdname debias_helpers
#' @keywords internal
#' Build an observation vector aligned to X's columns for time `t_idx`.
debias_obs_vec_for_time <- function(t_idx, site_index, col_vars, obs.mean, name_map = debias_name_map) {
  om  <- obs.mean[[t_idx]]
  out <- rep(NA_real_, length(col_vars))
  
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

# ------------------------------------------------------------------------------
# (5) Diagnostics
# ------------------------------------------------------------------------------

#' @rdname debias_helpers
#' @keywords internal
#' Assemble a comparison table per column (site/var) with pre/post/obs values.
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

#' @rdname debias_helpers
#' @keywords internal
#' Compute RMSE by state variable comparing pre/post vs. obs.
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

# ------------------------------------------------------------------------------
# (6) Per-step debias application (uses site filtering + covariates)
# ------------------------------------------------------------------------------

#' Apply residual debiasing for a single SDA time step (internal)
#'
#' Trains/updates per-variable residual models using data from t-1 and predicts
#' residuals at t, then mean-shifts the ensemble accordingly. Also returns
#' diagnostics and learner weights for logging.
#' @keywords internal
#' @noRd
sda_apply_debias_step <- function(
    t, obs.t, X, raw_prev, raw_mean_t,
    site_index, col_vars,
    obs.times, obs.mean,
    covariates_df, py, train_buf,
    name_map = debias_name_map,
    drop_incomplete_covariates = TRUE,
    enforce_consistent_obs = TRUE,
    require_obs_at_t_for_predict = FALSE
) {
  # Guard
  if (t <= 1 || is.null(covariates_df)) {
    return(list(
      X = X,
      weights_entry   = NULL,
      weights_df_rows = utils::head(data.frame(time=character(), var=character(), learner=character(), weight=numeric()), 0),
      diag = list(
        comp = debias_build_comp_df(site_index, col_vars, raw_mean_t, raw_mean_t, rep(NA_real_, length(col_vars))),
        rmse = data.frame(
          var       = unique(col_vars),
          rmse_pre  = NA_real_, rmse_post = NA_real_,
          mae_pre   = NA_real_, mae_post  = NA_real_,
          bias_pre  = NA_real_, bias_post = NA_real_,
          r2_pre    = NA_real_, r2_post   = NA_real_
        )
      ),
      rmse_rows = utils::head(data.frame(
        time=character(), var=character(),
        rmse_pre=numeric(), rmse_post=numeric(),
        mae_pre=numeric(),  mae_post=numeric(),
        bias_pre=numeric(), bias_post=numeric(),
        r2_pre=numeric(),   r2_post=numeric()
      ), 0)
    ))
  }
  
  # Build obs + covariates
  obs_prev_vec <- debias_obs_vec_for_time(t - 1, site_index, col_vars, obs.mean, name_map)
  
  cov_prev_mat <- debias_cov_by_columns(
    covariates_df = covariates_df, obs_date = obs.times[t - 1],
    site_index = site_index, obs.mean = obs.mean, t_idx = t - 1,
    drop_incomplete_covariates = drop_incomplete_covariates,
    enforce_consistent_obs     = enforce_consistent_obs
  )
  cov_t_mat <- debias_cov_by_columns(
    covariates_df = covariates_df, obs_date = obs.times[t],
    site_index = site_index, obs.mean = obs.mean, t_idx = t,
    drop_incomplete_covariates = drop_incomplete_covariates,
    enforce_consistent_obs     = enforce_consistent_obs
  )
  
  if (ncol(cov_prev_mat) == 0 || ncol(cov_t_mat) == 0) {
    return(list(
      X = X,
      weights_entry   = NULL,
      weights_df_rows = utils::head(data.frame(time=character(), var=character(), learner=character(), weight=numeric()), 0),
      diag = list(
        comp = debias_build_comp_df(site_index, col_vars, raw_mean_t, raw_mean_t, rep(NA_real_, length(col_vars))),
        rmse = data.frame(
          var       = unique(col_vars),
          rmse_pre  = NA_real_, rmse_post = NA_real_,
          mae_pre   = NA_real_, mae_post  = NA_real_,
          bias_pre  = NA_real_, bias_post = NA_real_,
          r2_pre    = NA_real_, r2_post   = NA_real_
        )
      ),
      rmse_rows = utils::head(data.frame(
        time=character(), var=character(),
        rmse_pre=numeric(), rmse_post=numeric(),
        mae_pre=numeric(),  mae_post=numeric(),
        bias_pre=numeric(), bias_post=numeric(),
        r2_pre=numeric(),   r2_post=numeric()
      ), 0)
    ))
  }
  
  pred_resid <- numeric(ncol(X))
  vars <- unique(col_vars)
  weights_entry <- list()
  weights_df_rows <- utils::head(
    data.frame(time=character(), var=character(), learner=character(), weight=numeric(), stringsAsFactors = FALSE), 0
  )
  
  add_weight_rows <- function(time_label, var, w_named) {
    if (is.null(names(w_named))) names(w_named) <- paste0("learner_", seq_along(w_named))
    data.frame(
      time    = rep(time_label, length(w_named)),
      var     = rep(var,        length(w_named)),
      learner = names(w_named),
      weight  = as.numeric(w_named),
      stringsAsFactors = FALSE
    )
  }
  
  # Optionally require obs at t to predict
  obs_t_avail <- if (require_obs_at_t_for_predict) {
    !is.na(debias_obs_vec_for_time(t, site_index, col_vars, obs.mean, name_map))
  } else rep(TRUE, length(col_vars))
  
  for (v in vars) {
    cols_v    <- which(col_vars == v)
    y_v_all   <- obs_prev_vec[cols_v] - as.numeric(raw_prev[cols_v])
    Xprev_all <- cbind(cov_prev_mat[cols_v, , drop = FALSE],
                       raw = as.numeric(raw_prev[cols_v]))
    mask <- !is.na(y_v_all) & stats::complete.cases(Xprev_all)
    
    if (any(mask)) {
      rec <- if (exists(v, train_buf, inherits = FALSE)) get(v, train_buf) else list(X = NULL, y = NULL)
      rec$X <- rbind(rec$X, Xprev_all[mask, , drop = FALSE])
      rec$y <- c(rec$y,  y_v_all[mask])
      assign(v, rec, train_buf)
      
      py$train_full_model(name = as.character(v),
                          X = as.matrix(rec$X),
                          y = as.numeric(rec$y))
      
      w_now <- try(py$get_model_weights(as.character(v)), silent = TRUE)
      if (!inherits(w_now, "try-error") && !is.null(w_now) && is.finite(w_now)) {
        w_now <- min(max(as.numeric(w_now), 0), 1)
        w_named <- c(KNN = w_now, TREE = 1 - w_now)
        weights_entry[[as.character(v)]] <- w_named
        weights_df_rows <- rbind(weights_df_rows, add_weight_rows(obs.t, as.character(v), w_named))
      }
    }
    
    if (py$has_model(as.character(v))) {
      Xt_v <- cbind(cov_t_mat[cols_v, , drop = FALSE],
                    raw = as.numeric(raw_mean_t[cols_v]))
      ok <- stats::complete.cases(Xt_v) & obs_t_avail[cols_v]
      if (any(ok)) {
        preds <- py$predict_residual(as.character(v), Xt_v[ok, , drop = FALSE])
        pred_resid[cols_v[ok]] <- as.numeric(preds)
      }
    }
  }
  
  pred_resid[!is.finite(pred_resid)] <- 0
  
  pre_mean  <- raw_mean_t
  post_mean <- raw_mean_t + pred_resid
  obs_t_vec <- debias_obs_vec_for_time(t, site_index, col_vars, obs.mean, name_map)  # <— keep this
  
  comp_df  <- debias_build_comp_df(site_index, col_vars, pre_mean, post_mean, obs_t_vec)
  
  metric_one <- function(pred, obs) {
    ok   <- is.finite(pred) & is.finite(obs)
    if (!any(ok)) return(c(rmse=NA_real_, mae=NA_real_, bias=NA_real_, r2=NA_real_))
    e    <- pred[ok] - obs[ok]
    rmse <- sqrt(mean(e^2)); mae <- mean(abs(e)); bias <- mean(e)
    sst  <- sum((obs[ok] - mean(obs[ok]))^2)
    r2   <- if (sst > 0 && sum(ok) >= 2) 1 - sum(e^2) / sst else NA_real_
    c(rmse=rmse, mae=mae, bias=bias, r2=r2)
  }
  
  metrics_by_var <- do.call(
    rbind,
    lapply(split(comp_df, comp_df$var), function(d) {
      m_pre  <- metric_one(d$pre,  d$obs)
      m_post <- metric_one(d$post, d$obs)
      data.frame(
        var        = d$var[1],
        rmse_pre   = m_pre["rmse"],  rmse_post = m_post["rmse"],
        mae_pre    = m_pre["mae"],   mae_post  = m_post["mae"],
        bias_pre   = m_pre["bias"],  bias_post = m_post["bias"],
        r2_pre     = m_pre["r2"],    r2_post   = m_post["r2"],
        stringsAsFactors = FALSE
      )
    })
  )
  diag_metrics <- metrics_by_var
  metrics_by_var$time <- obs.t
  rmse_rows <- metrics_by_var[, c("time","var","rmse_pre","rmse_post","mae_pre","mae_post","bias_pre","bias_post","r2_pre","r2_post")]
  
  # Mean-shift ensemble
  offsets   <- sweep(X, 2, raw_mean_t, FUN = "-")
  corrected <- post_mean
  X_new     <- sweep(offsets, 2, corrected, FUN = "+")
  
  list(
    X = X_new,
    weights_entry   = if (length(weights_entry)) weights_entry else NULL,
    weights_df_rows = weights_df_rows,
    diag            = list(comp = comp_df, rmse = diag_metrics),
    rmse_rows       = rmse_rows
  )
}

