#' Read soil parameters from a PEcAn soil physics file
#'
#' Reads values from a netCDF that follows the PEcAn soil standard,
#'   aggregates layers across the depth to be simulated,
#'   and converts to names and units expected by RothC.
#'
#' TODO: Currently drops all of any layer that extends below `model_depth`.
#'   It would be better instead to weight all layers by their contribution
#'   to the requested depth.
#'
#' @param path filepath to a netcdf,
#'   probably read from `settings$run$inputs$soil_physics$path`
#' @param model_depth Soil depth to be simulated, in cm
#'
#' @return one-row dataframe with columns
#'   `depth_cm`, `clay_pct`, `silt_pct`,
#'   `bulkdens_g_cm3`, `org_C_pct`, `iom_tC_ha`
#'
#' @importFrom rlang .data .env
#'
read_soil_physics <- function(path, model_depth = 23) {
  soil_list <- PEcAn.utils::netcdf2df(path)

  # Input is documented to be in meters,
  # but providing cm instead seems to be a _very_ common error;
  # might as well try to handle it here
  if (all(soil_list$dims$depth < 10)) {
    depth_cm <- PEcAn.utils::ud_convert(soil_list$dims$depth, "m", "cm")
  } else {
    PEcAn.logger::logger.warn(
      "Soil depths should be in meters, but found values >= 10",
      "in file", path,
      "Assuming these are mislabeled cm and treating them as such."
    )
    depth_cm <- soil_list$dims$depth
  }
  soil_list$vals |>
    as.data.frame() |>
    # TODO: Assumes depth is given to bottom of layer -- is that correct?
    dplyr::mutate(depth_cm = .env$depth_cm) |>
    # TODO this drops layers that extend past bottom
    # (eg with depth=23 and 0-10/10-30 layering, would use only 0-10)
    # Consider rescaling partial layers
    # (Or throwing an error on mismatch and making everyone generate their soil
    #  files with layers that match model depth?)
    dplyr::filter(.data$depth_cm <= model_depth) |>
    dplyr::summarize(
      # TODO consider weighting by layer thickness?
      depth_cm = max(.data$depth_cm),
      clay_pct = .data$fraction_of_clay_in_soil |>
        mean() |>
        PEcAn.utils::ud_convert("1", "%"),
      silt_pct = .data$fraction_of_silt_in_soil |>
        mean() |>
        PEcAn.utils::ud_convert("1", "%"),
      bulkdens_g_cm3 = .data$soil_bulk_density |>
        mean() |>
        PEcAn.utils::ud_convert("kg m-3", "g cm-3"),
      org_C_pct = .data$soil_organic_carbon_stock |>
        sum() |>
        PEcAn.utils::ud_convert("kg m-2", "g cm-2") |>
        (\(x) x / (.data$depth_cm * .data$bulkdens_g_cm3))() |>
        PEcAn.utils::ud_convert("1", "%"),
      iom_tC_ha = .data$soil_organic_carbon_stock |>
        sum() |>
        PEcAn.utils::ud_convert("kg m-2", "t ha-1") |>
        # Approximation from Falloon et al. 1998, 10.1016/S0038-0717(97)00256-3
        (\(x) 0.049 * x^1.139)()
    )
}
