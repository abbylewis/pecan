library(expm)

predict_k_steps = function(tmat, current_state, k) {
  if (!is.matrix(tmat)) tmat = as.matrix(tmat)
  if (is.null(rownames(tmat)) || is.null(colnames(tmat)))
    stop("tmat must have row/col names.")
  if (!(current_state %in% rownames(tmat)))
    stop("State not in matrix.")
  
  init = rep(0, nrow(tmat)); names(init) = rownames(tmat)
  init[current_state] = 1
  
  pk = tmat %^% k
  out = as.numeric(init %*% pk)
  names(out) = colnames(tmat)
  out
}

get_state_at = function(pid, seq_long_dt, yr, season_idx) {
  st = seq_long_dt[parcel_id == pid & year == yr & season == season_idx, CLASS]
  if (length(st) == 0) stop("No observation for this parcel at that (year, season).")
  if (length(st) > 1) stop("Multiple rows found; check duplicates.")
  st
}

# one prediction per year for focus season, anchored at latest observed year for that season
predict_yearly = function(tmat, pid, seq_long_dt,
                                  end_year, season_idx,
                                  anchor_year = NULL,
                                  return_probs = FALSE) {
  
  if (!is.matrix(tmat)) tmat = as.matrix(tmat)
  stopifnot(all(rownames(tmat) == colnames(tmat)))
  
  season_idx = as.integer(season_idx)
  if (!(season_idx %in% 1:4)) stop("season_idx must be 1..4")
  
  # find anchor year (latest observed year for that parcel+season), 
  if (is.null(anchor_year)) {
    yrs_avail = seq_long_dt[parcel_id == pid & season == season_idx, unique(year)]
    if (length(yrs_avail) == 0) stop("No observations for this parcel_id at that season.")
    anchor_year = max(yrs_avail, na.rm = TRUE)
  }
  
  cur = get_state_at(pid, seq_long_dt, anchor_year, season_idx)
  
  years = seq.int(anchor_year, end_year)
  k_vec = (years - anchor_year) * 4   # same season each year = 4 steps per year
  
  preds = character(length(years))
  top_p = numeric(length(years))
  probs_list = if (return_probs) vector("list", length(years)) else NULL
  
  for (i in seq_along(years)) {
    k = k_vec[i]
    
    if (k == 0) {
      p = rep(0, nrow(tmat)); names(p) = rownames(tmat); p[cur] = 1
    } else {
      p = predict_k_steps(tmat, cur, k)
    }
    
    preds[i] = names(which.max(p))
    top_p[i] = max(p)
    if (return_probs) probs_list[[i]] = p
  }
  
  out = data.table(
    parcel_id = pid,
    season = season_idx,
    year = years,
    steps_ahead = k_vec,
    anchor_year = anchor_year,
    anchor_state = cur,
    pred_class = preds,
    pred_prob = top_p
  )
  if (return_probs) out[, probs := probs_list]
  out
}

#anchor at latest observed year for season s, then predict to future year
test = predict_yearly(
  tmat = tmat_final,
  pid = "1",
  seq_long_dt = seq_long,
  end_year = 2030,
  season_idx = 3,
  return_probs = TRUE
)

print(test)
paste(test$pred_class, collapse = "-")
