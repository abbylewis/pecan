split_into_batches <- function(x, batch_size) {
  split(x, ceiling(seq_along(x) / batch_size))
}

make_crop_timeseries <- function(crops_with_soil, phenology, precip, etref) {
  crop_cols <- c(
    "parcel_id",
    "year",
    "crop_name",
    "whc_min_frac",
    "whc_mm",
    "whc_min_frac"
  )

  crop_soil_timeseries <- crops_with_soil |>
    dplyr::select(dplyr::all_of(crop_cols)) |>
    dplyr::inner_join(
      phenology,
      by = c("parcel_id", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::slice_max(
      .data$canopy_cover,
      n = 1,
      by = c("parcel_id", "date")
    )

  check_unique <- crop_soil_timeseries |>
    dplyr::group_by(.data$parcel_id, .data$date) |>
    dplyr::count() |>
    dplyr::filter(.data$n > 1)
  if (nrow(check_unique) > 1) {
    bad_parcels <- unique(check_unique[["parcel_id"]])
    warning(
      "The parcels below have some non-unique values ",
      "even after `slice_max(canopy_cover)`. ",
      "This is likely because of non-unique ",
      "landIQ crop --> crop_type mappings. ",
      "Selecting only the first row in each of these cases.",
      "\n",
      paste(bad_parcels, collapse = ", ")
    )
    crop_soil_timeseries <- crop_soil_timeseries |>
      dplyr::slice_max(
        .data$canopy_cover,
        n = 1,
        by = c("parcel_id", "date"),
        with_ties = FALSE
      )
  }

  complete_crop_timeseries <- crop_soil_timeseries |>
    dplyr::left_join(precip, by = c("parcel_id", "date")) |>
    dplyr::left_join(
      dplyr::select(etref, -"year"),
      by = c("parcel_id", "date")
    ) |>
    dplyr::arrange(.data$parcel_id, .data$date) |>
    tidyr::fill("etref_mm_day") |>
    dplyr::mutate(
      etc_mm_day = eto_to_etc_bism(
        eto = .data$etref_mm_day,
        crop_name = .data$crop_name[[1]],
        date = .data$date
      ),
      .by = "crop_name"
    )

  complete_crop_timeseries
}

make_event_df <- function(parcel_waterbalance, outfile) {
  pw_sub <- parcel_waterbalance |>
    dplyr::filter(.data$irr > 0) |>
    dplyr::relocate("parcel_id", "date", "crop_name", "canopy_cover", "irr")

  irr_events <- pw_sub |>
    dplyr::select(
      "parcel_id",
      "date",
      "crop_name",
      amount_mm = "irr",
      "canopy_cover"
    ) |>
    dplyr::mutate(
      crop_code = crop_name,
      method = dplyr::case_when(
        crop_code == "Rice" ~ "flood",
        TRUE ~ "canopy"
      )
    ) |>
    dplyr::select("parcel_id", "date", "amount_mm", "method")

  arrow::write_parquet(irr_events, outfile)
  invisible(outfile)
}
