#' Extract the first planting date of each crop cycle
#'
#' Reads a (JSON) management events file and finds the planting events at which
#' the site changes from from one crop to another, ignoring repeat plantings of
#' the same crop.
#' These are the dates when single-PFT models will need to restart to update
#' their crop parameterization.
#'
#' TODO: For now this function requires each planting event to specify a
#' `crop` attribute, but note that this is not enforced by v0.1 of the PEcAn
#' events schema. The schema instead allows each site object to specify a
#' site-level `PFT` attribute that is implied constant over time.
#' As I write this I think the schema may need to change to require a crop or
#' PFT identifier be specified for every planting event.
#'
#' @param event_json path to an `events.json` file
#'
#' @return data frame with columns `site_id`, `date`, `crop`,
#'  with one row per detected crop cycle.
#' @export
#' @author Chris Black
#'
#' @examples
#' # Not currently runnable because file does not list crop in planting events.
#' # Revisit after deciding if schema update is warranted.
#' \dontrun{
#' evts <- system.file(
#'   "events_fixtures/events_site1_site2.json",
#'   package = "PEcAn.data.land"
#' )
#' events_to_crop_cycle_starts(evts)
#' }
events_to_crop_cycle_starts <- function(event_json) {
  event_df <- jsonlite::read_json(event_json) |>
    dplyr::bind_rows() |>
    dplyr::mutate(events = purrr::map(.data$events, as.data.frame)) |>
    tidyr::unnest("events") |>
    dplyr::filter(event_type %in% c("planting", "harvest")) |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::arrange(.data$date)
  # We should always have alternating planting-harvest-planting-harvest cycles.
  # If we don't, raise a warning.
  dup_events <- event_df |>
    dplyr::filter(
      (.data$event_type == dplyr::lag(.data$event_type)) |
        (.data$event_type == dplyr::lead(.data$event_type))
    ) |>
    dplyr::select("site_id", "date", "event_type", "crop_code")
  if ((ndup <- nrow(dup_events)) > 0) {
    dfdump <- paste("# ", capture.output(print(dup_events)), collapse = "\n")
    PEcAn.logger::logger.warn(
      "Detected", ndup, "instances of non-alternating harvest-planting cycles:",
      "\n", dfdump,
      wrap = FALSE
    )
  }
  find_crop_changes(event_df)
}

# helper for events_to_crop_cyle_starts,
# mostly to ease unit testing
find_crop_changes <- function(event_df) {
  event_df |>
    dplyr::filter(.data$event_type == "planting") |>
    dplyr::arrange(.data$site_id, .data$date) |>
    dplyr::mutate(crop_cycle_id = dplyr::consecutive_id(.data$site_id, .data$crop_code)) |>
    dplyr::group_by(.data$site_id, .data$crop_cycle_id) |>
    dplyr::slice_min(.data$date) |>
    dplyr::ungroup() |>
    dplyr::select("site_id", "date", "crop_code")
}
