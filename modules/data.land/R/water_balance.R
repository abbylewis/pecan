#' Calculate water balance for a time series at a single site
#'
#' This is the core water balance calculation that operates on primitive
#' numeric vectors for easy testing and debugging. Each input is a time series
#' of daily values for a single location (one date per row). The units for all
#' quantities are arbitrary, but they should be consistent (distance / time;
#' e.g., most commonly, mm/day).
#'
#' This function operates in *relative* WHC space, where:
#' - `w = 0` represents wilting point (no plant-available water)
#' - `w = whc` represents field capacity (maximum plant-available water)
#' - `whc = (field_capacity - wilting_point) * rooting_depth` (the plant-available range)
#'
#' The handling of rice here is crude and primitive: setting `whc_min_frac` =
#' 1.0 (as set in `crop_whc` for rice) means a near-constant need for
#' irrigation to balance ET + seepage, which roughly mimics the behavior of
#' maintaining a standing flood. However, proper treatment of rice requires
#' maintaining a field *above* field capacity, and has other complications.
#' These will be implemented in the future.
#'
#' @param et Vector of evapotranspiration values (distance / time)
#' @param precip Vector of precipitation values (distance / time)
#' @param whc Water holding capacity (distance); the plant-available range from
#'   wilting point to field capacity (i.e., `whc = field_capacity - wilting_point`)
#' @param whc_min_frac Fraction of WHC for minimum water level (irrigation
#'   trigger); unused if `w_min` is explicitly specified
#' @param W_initial Initial soil water content at start of time series
#'   (distance); defaults to `whc` (field capacity) if NULL
#' @param w_min Minimum water level threshold (distance); irrigation is
#'   triggered when soil water falls below this level; defaults to
#'   `whc_min_frac * whc` if NULL
#' @param seepage_rate Daily seepage loss for rice paddies (distance / time);
#'   only used when `is_rice = TRUE`
#' @param is_rice Logical; if TRUE, applies a constant seepage loss (mm/day)
#' @return List with vectors: W_t (soil water), irr (irrigation), runoff
#' @examples
#' # Calculate WHC from field capacity, wilting point, and rooting depth
#' field_capacity <- 0.30  # volumetric (m3/m3)
#' wilting_point <- 0.10   # volumetric (m3/m3)
#' rooting_depth <- 1000   # mm
#' whc <- (field_capacity - wilting_point) * rooting_depth  # mm
#'
#' # Run water balance with 5 days of ET and precip data
#' et <- c(4, 5, 6, 4, 3)  # mm/day
#' precip <- c(0, 0, 10, 0, 0)  # mm/day
#' result <- calc_water_balance(et, precip, whc = whc, whc_min_frac = 0.5)
#' str(result)
#' @export
calc_water_balance <- function(
  et,
  precip,
  whc,
  whc_min_frac,
  W_initial = NULL, #nolint: object_name_linter
  w_min = NULL,
  seepage_rate = NULL,
  is_rice = FALSE
) {
  # nolint start: object_name_linter
  n <- length(et)

  if (!length(precip) == n) {
    PEcAn.logger::logger.severe(
      "Precip and ET have different lengths. ",
      "length(precip) = ", length(precip), "  ",
      "length(et) = ", n
    )
  }

  if (!length(whc) == 1) {
    PEcAn.logger::logger.severe(
      "whc must have length 1; actual length = ", length(whc)
    )
  }

  if (!length(whc_min_frac) == 1) {
    PEcAn.logger::logger.severe(
      "whc_min_frac must have length 1; actual length = ", length(whc_min_frac)
    )
  }

  if (is_rice && is.null(seepage_rate)) {
    PEcAn.logger::logger.severe("Seepage rate must be defined for rice fields")
  }

  if (is.null(w_min)) {
    w_min <- whc_min_frac * whc
  }

  if (is.null(W_initial)) {
    # Initialize at field capacity (i.e., full WHC)
    W_prev <- whc
  } else {
    W_prev <- W_initial
  }

  W_t <- numeric(n)
  irr <- numeric(n)
  runoff <- numeric(n)

  for (t in seq_len(n)) {
    # Only water above w_min is available for seepage
    seepage <- if (is_rice) min(seepage_rate, max(0, W_prev - w_min)) else 0.0

    # Potential state after precip and ET
    W0 <- W_prev + precip[t] - et[t] - seepage

    # If W0 falls below w_min (e.g., high ET and seepage; low precip), irrigate
    # to field capacity (i.e., full WHC).
    if (W0 < w_min) {
      irr[t] <- whc - W0
      W0 <- whc
    } else {
      irr[t] <- 0
    }

    # If W0 exceeds field capacity (i.e., whc), the difference is runoff.
    if (W0 > whc) {
      runoff[t] <- W0 - whc
      W_t[t] <- whc
    } else {
      runoff[t] <- 0
      W_t[t] <- max(W0, w_min)
    }

    W_prev <- W_t[t]
  }

  # nolint end: object_name_linter

  list(W_t = W_t, irr = irr, runoff = runoff)
}

#' Apply water balance calculations to a data frame with multiple sites
#'
#' Groups by location and applies calc_water_balance to each group. Unlike
#' `calc_water_balance`, the units here *do* matter -- they should be `mm_day`.
#'
#' @param df Data frame with columns: `date`, `location_id`, `etc_mm_day`,
#' `precip_mm_day`, and `whc_min_frac`
#' @param idcol Column name for grouping (typically, `location_id`, `parcel_id` or similar)
#' @param whc_mm Water holding capacity (mm)
#' @return Data frame with added columns: `W_t`, `irr`, `runoff`
#' @export
apply_water_balance <- function(df, idcol, whc_mm = 500) {
  need_cols <- c("etc_mm_day", "precip_mm_day", "date")
  missing_cols <- need_cols[!(need_cols %in% colnames(df))]
  default_whc_min_frac <- 0.375
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Missing the following required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if ("whc_min_frac" %in% colnames(df)) {
    n_na <- sum(is.na(df$whc_min_frac))
    if (n_na > 0) {
      PEcAn.logger::logger.warn(
        sprintf(
          "whc_min_frac has %d NA values. Replacing with default = %.3f",
          n_na,
          default_whc_min_frac
        )
      )
    }
  } else {
    PEcAn.logger::logger.warn(
      "whc_min_frac column not found in input data. Using default = ",
      default_whc_min_frac
    )
    df[["whc_min_frac"]] <- default_whc_min_frac
  }

  df |>
    dplyr::arrange(.data[[idcol]], .data$date) |> # nolint: object_usage_linter
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      week = as.integer(format(.data$date, "%U")),
      day_of_year = as.integer(format(.data$date, "%j")),
      whc_min_frac = tidyr::replace_na(.data$whc_min_frac, default_whc_min_frac),
      results = tibble::as_tibble(calc_water_balance(
        et = .data$etc_mm_day,
        precip = .data$precip_mm_day,
        whc = whc_mm,
        # NOTE: Use unique here because, in a merged crop data frame, this gets
        # expanded to a vector of values. They *should* all be unique per
        # `idcol`; if they're not, `calc_water_balance` should fail loudly.
        whc_min_frac = unique(.data$whc_min_frac)
      )),
      .by = dplyr::all_of(idcol)
    ) |>
    tidyr::unpack(.data$results)
}
