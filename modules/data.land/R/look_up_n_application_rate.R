#' Look up N application rates by crop
#'
#' Returns recommended nitrogen application rate ranges for California crops,
#' based on CDFA-FREP guidelines and UC ANR publications. Rates are provided
#' in both imperial (lbs N/acre) and SI (kg N/m2) units.
#'
#' When multiple crops match, all matching rows are returned. When no crop
#' matches, an empty tibble is returned and a warning is issued.
#'
#' @param crop Character string. Crop name to look up. Matching is
#'   case-insensitive and supports partial matching (e.g. "tomato" matches
#'   "Tomatoes, fresh market" and "Tomatoes, processing").
#' @param pft_group Optional character string. Filter results to a specific
#'   plant functional type group (e.g. "row", "woody", "rice").
#' @param unit Character, one of "kg_m2" (default) or "lbs_acre". Controls
#'   which columns are included in the output.
#'
#' @return A tibble with columns: `pft_group`, `crop`, `min_n`, `max_n`,
#'   `source`. The `min_n` and `max_n` columns are in the requested unit.
#'
#' @examples
#' look_up_n_application_rate("corn")
#' look_up_n_application_rate("tomato")
#' look_up_n_application_rate("wheat", unit = "lbs_acre")
#' look_up_n_application_rate("pistachio", pft_group = "woody")
#'
#' @export
look_up_n_application_rate <- function(
    crop,
    pft_group = NULL,
    unit = c("kg_m2", "lbs_acre")
) {
  unit <- match.arg(unit)

  crop_lower <- tolower(crop)
  result <- PEcAn.data.land::n_application_rate_data |>
    dplyr::filter(grepl(crop_lower, tolower(.data$crop), fixed = TRUE))

  if (!is.null(pft_group)) {
    result <- result |>
      dplyr::filter(tolower(.data$pft_group) == tolower(pft_group))
  }

  if (nrow(result) == 0) {
    PEcAn.logger::logger.warn(
      "No N application rate found for crop '", crop, "'"
    )
    return(data.frame(
      pft_group = character(),
      crop = character(),
      min_n = numeric(),
      max_n = numeric(),
      source = character(),
      stringsAsFactors = FALSE
    ))
  }

  if (unit == "kg_m2") {
    result |>
      dplyr::transmute(
        .data$pft_group,
        .data$crop,
        min_n = .data$min_n_kg_m2,
        max_n = .data$max_n_kg_m2,
        .data$source
      )
  } else {
    result |>
      dplyr::transmute(
        .data$pft_group,
        .data$crop,
        min_n = .data$min_n_lbs_acre,
        max_n = .data$max_n_lbs_acre,
        .data$source
      )
  }
}


#' Look up compost amendment properties
#'
#' Returns properties of organic amendment materials including carbon and
#' nitrogen content, C:N ratio, and plant-available nitrogen (PAN).
#'
#' @param material Character string. Amendment material to look up.
#'   Case-insensitive partial matching (e.g. "cow" matches "Cow manure").
#' @param n_class Optional, one of "LOWER" or "HIGHER". Filter by N class.
#'
#' @return A tibble with columns: `material`, `cn_avg`, `c_pct`, `n_pct`,
#'   `pan_pct`, `n_class`, `total_c_min_kg_m2`, `total_c_max_kg_m2`,
#'   `total_n_min_kg_m2`, `total_n_max_kg_m2`.
#'
#' @examples
#' look_up_compost_amendment("cow manure")
#' look_up_compost_amendment("poultry", n_class = "HIGHER")
#'
#' @export
look_up_compost_amendment <- function(material, n_class = NULL) {
  mat_lower <- tolower(material)
  result <- PEcAn.data.land::compost_amendment_data |>
    dplyr::filter(grepl(mat_lower, tolower(.data$material), fixed = TRUE))

  if (!is.null(n_class)) {
    result <- result |>
      dplyr::filter(toupper(.data$n_class) == toupper(n_class))
  }

  if (nrow(result) == 0) {
    PEcAn.logger::logger.warn(
      "No compost amendment found for material '", material, "'"
    )
    return(data.frame(
      material = character(), cn_avg = numeric(), c_pct = numeric(),
      n_pct = numeric(), pan_pct = numeric(), n_class = character(),
      total_c_min_kg_m2 = numeric(), total_c_max_kg_m2 = numeric(),
      total_n_min_kg_m2 = numeric(), total_n_max_kg_m2 = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  result |>
    dplyr::select(
      "material", "cn_avg", "c_pct", "n_pct", "pan_pct", "n_class",
      "total_c_min_kg_m2", "total_c_max_kg_m2",
      "total_n_min_kg_m2", "total_n_max_kg_m2"
    )
}
