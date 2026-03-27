#!/usr/bin/env Rscript

# devtools::install("~/projects/pecan/sipnet-events/modules/data.remote", upgrade = FALSE)
# devtools::install("~/projects/pecan/sipnet-events/base/workflow", upgrade = FALSE)
# devtools::install("~/projects/pecan/sipnet-events/models/sipnet", upgrade = FALSE)

# devtools::load_all("~/projects/pecan/sipnet-events/modules/data.land")
# devtools::load_all("~/projects/pecan/sipnet-events/models/sipnet")

# Pick a parcel from irrigation
pid <- 39011

irrigation_path <- "/projectnb/dietzelab/ccmmf/usr/ashiklom/event-outputs/irrigation_10000.parquet"

# Find the closest design point to that parcel to use existing met
parcel_path <- "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1/parcels.gpkg"
parcel <- sf::read_sf(
  parcel_path,
  query = glue::glue("SELECT * FROM parcels WHERE parcel_id = {pid}")
)

dp_path <- "/projectnb/dietzelab/ccmmf/management/irrigation/design_points.csv"
design_points <- read.csv(dp_path) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(sf::st_crs(parcel))
dp_idx <- sf::st_nearest_feature(parcel, design_points)
site_id <- design_points[dp_idx, ][["id"]]

# site1 <- "~/projects/pecan/sipnet-events/modules/data.land/inst/events_fixtures/events_site1.json"
# site1_multi <- "~/projects/pecan/sipnet-events/modules/data.land/inst/events_fixtures/events_site1_multi.json"
# site_12 <- "~/projects/pecan/sipnet-events/modules/data.land/inst/events_fixtures/events_site1_site2.json"

binary <- "/projectnb/dietzelab/ccmmf/usr/ashiklom/pecan/sipnet/sipnet"
met <- file.path(
  "/projectnb/dietzelab/ccmmf/ensemble/ERA5_SIPNET",
  site_id,
  "ERA5.1.2016-01-01.2024-12-31.clim"
)
stopifnot(file.exists(met))

# Make the events.json
planting <- fs::dir_ls(
  "/projectnb/dietzelab/ccmmf/management/event_files",
  regexp = "planting_statewide_.*\\.parquet"
) |>
  arrow::open_dataset() |>
  dplyr::collect() |>
  dplyr::filter(.data$site_id == as.character(.env$pid)) |>
  dplyr::mutate(date = as.Date(.data$date)) |>
  tibble::as_tibble()

code_pft_mapping <- planting |>
  dplyr::distinct(crop_code = .data$code, .data$PFT)

planting_events <- planting |>
  dplyr::select(
    "site_id", "event_type", "date",
    "crop_code" = "code",
    "leaf_c_kg_m2" = "C_LEAF",
    "wood_c_kg_m2" = "C_STEM",
    "fine_root_c_kg_m2" = "C_FINEROOT",
    "coarse_root_c_kg_m2" = "C_COARSEROOT",
    "leaf_n_kg_m2" = "N_LEAF",
    "wood_n_kg_m2" = "N_STEM",
    "fine_root_n_kg_m2" = "N_FINEROOT",
    "coarse_root_n_kg_m2" = "N_COARSEROOT"
  )

# Harvest
mslsp_path <- "/projectnb/dietzelab/ccmmf/management/phenology/matched_landiq_mslsp_v4.1"
phenology <- fs::dir_ls(mslsp_path, glob = "*.parquet") |>
  arrow::open_dataset() |>
  dplyr::filter(.data$parcel_id == .env$pid, !is.na(.data$mslsp_cycle)) |>
  dplyr::collect() |>
  tibble::as_tibble() |>
  dplyr::arrange(.data$year, .data$mslsp_cycle) |>
  dplyr::relocate(
    "year", "mslsp_cycle", dplyr::starts_with("landiq_"),
  )

# Dummy values for testing
harvest_events <- phenology |>
  dplyr::mutate(
    event_type = "harvest",
    site_id = as.character(.data$parcel_id),
    frac_above_removed_0to1 = 0.85
  ) |>
  dplyr::select(
    "site_id", "event_type", "date" = mslsp_OGMn, "frac_above_removed_0to1"
  )

start_date <- min(planting$date)
end_date <- max(harvest$date)

irrigation_events <- arrow::open_dataset(irrigation_path) |>
  dplyr::filter(
    .data$parcel_id == .env$pid,
    .data$ens_id == "irr_ens_001"
  ) |>
  dplyr::select(-"ens_id") |>
  dplyr::collect() |>
  tibble::as_tibble() |>
  dplyr::filter(.data$date <= .env$end_date) |>
  dplyr::mutate(
    event_type = "irrigation",
    site_id = as.character(.data$parcel_id),
    .keep = "unused"
  ) |>
  dplyr::relocate("site_id", "event_type", "date")

make_event_list <- function(df) {
  df2list <- function(df) {
    as.list(df) |> purrr::list_transpose()
  }
  df |>
    tidyr::nest(.by = "site_id", .key = "events") |>
    dplyr::mutate(events = purrr::map(.data$events, df2list))
}

planting_n <- make_event_list(planting_events)
harvest_n <- make_event_list(harvest_events)
irrigation_n <- make_event_list(irrigation_events)
all_events <- planting_n |>
  dplyr::full_join(harvest_n, by = "site_id") |>
  dplyr::full_join(irrigation_n, by = "site_id") |>
  dplyr::mutate(
    events = dplyr::starts_with("events") |>
      dplyr::across() |>
      purrr::pmap(c) |>
      purrr::map(unname),
    .keep = "unused"
  )

jsonlite::toJSON(all_events, pretty = TRUE, auto_unbox = TRUE)

################################################################################
events_json <- site1

outdir <- normalizePath("_test/segments")
dir.create(outdir, showWarnings = FALSE)

settings <- PEcAn.settings::as.Settings(list(
  outdir = file.path(outdir, "out"),
  rundir = file.path(outdir, "run"),
  modeloutdir = file.path(outdir, "out"),
  pfts = list(list(
    name = "grassland",
    constants = list(num = 1)
  )),
  model = list(
    type = "SIPNET",
    binary = binary,
    revision = "v2"
  ),
  run = list(
    site = list(
      id = site_id,
      name = site_id,
      lat = 32.71585,
      lon = -115.47163
    ),
    start.date = start_date,
    end.date = end_date,
    inputs = list(met = list(path = met))
  ),
  host = list(
    name = "localhost"
  )
))

events <- jsonlite::fromJSON(events_json, simplifyVector = FALSE)
# TODO: Iterate over events
site_events_obj <- events[[1]]

site_id <- site_events_obj[["site_id"]]
site_events_list <- site_events_obj[["events"]]
site_events_common <- site_events_obj
site_events_common[["events"]] <- NULL

crop_cycles <- events_to_crop_cycle_starts(site1_multi)

# Empty example
# crop_cycles <- tibble::tibble(
#   site_id = character(0),
#   date = as.Date(NULL),
#   crop_code = character(0)
# )

# Get segments
segments <- tibble::tibble(
  start_date = c(start_date, crop_cycles[["date"]]),
  end_date = c(crop_cycles[["date"]] - 1, end_date)
) |>
  dplyr::mutate(segment_id = dplyr::row_number())

################################################################################

for (isegment in seq_len(nrow(segments))) {
  # isegment <- 1
  segment <- segments[isegment, ]
  segment_id <- sprintf("%03d", isegment)
  dstart <- segment[["start_date"]]
  dend <- segment[["end_date"]]

  segment_dir <- file.path(outdir, paste0("segment_", segment_id))
  if (dir.exists(segment_dir)) {
    unlink(segment_dir, recursive = TRUE)
  }
  dir.create(segment_dir, showWarnings = FALSE, recursive = TRUE)

  # Filter events to relevant dates
  events_sub <- site_events_list |>
    purrr::keep(~as.Date(.x[["date"]]) >= dstart) |>
    purrr::keep(~as.Date(.x[["date"]]) <= dend)

  # Segment-separated events file
  eventfile <- file.path(segment_dir, "events.json")
  segment_event_obj <- list(c(site_events_common, events = list(events_sub)) )
  jsonlite::write_json(segment_event_obj, eventfile, auto_unbox = TRUE)

  segment_eventfile <- PEcAn.SIPNET::write.events.SIPNET(eventfile, segment_dir)

  # Subset the met to only the dates in this segment. SIPNET does not respect
  # start/end date, only the dates in the .clim file.
  met_orig <- read.table(settings[[c("run", "inputs", "met", "path")]])
  met_segment <- met_orig |>
    # Create a date from the year + DOY
    dplyr::mutate(date = as.Date(paste0(V2, "-01-01")) + V3) |>
    dplyr::filter(date >= dstart, date <= dend) |>
    dplyr::select(-c("date"))
  met_segment_file <- file.path(segment_dir, "met.clim")
  write.table(
    met_segment,
    met_segment_file,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE
  )

  # Segment-specific settings
  segment_outdir <- file.path(segment_dir, "out")
  dir.create(segment_outdir, showWarnings = FALSE, recursive = TRUE)
  segment_rundir <- file.path(segment_dir, "run")
  dir.create(segment_rundir, showWarnings = FALSE, recursive = TRUE)
  segment_rundir_withid <- file.path(segment_rundir, segment_id)
  dir.create(segment_rundir_withid, showWarnings = FALSE, recursive = TRUE)

  segment_settings <- settings
  segment_settings[["outdir"]] <- segment_outdir
  segment_settings[["rundir"]] <- segment_rundir
  segment_settings[["modeloutdir"]] <- segment_rundir
  segment_settings[[c("run", "start.date")]] <- dstart
  segment_settings[[c("run", "end.date")]] <- dend
  segment_settings[[c("run", "inputs", "met", "path")]] <- met_segment_file
  segment_settings[[c("run", "inputs", "events")]] <- list(path = segment_eventfile)

  if (isegment > 1) {
    # For isegment > 1, we restart from the *previous* segment's restart.out
    segment_settings[[c("model", "restart_in")]] <- restart_out
  }
  # ...and now, define a new restart.out for *this* segment
  restart_out <- file.path(segment_dir, "restart.out")
  segment_settings[[c("model", "restart_out")]] <- restart_out

  # Write runs file
  writeLines(segment_id, file.path(segment_rundir, "runs.txt"))

  # TODO: Logic to get the trait values corresponding to the segment's PFT.
  # 1. Cross-reference crop_code against PFT
  # 2. Get traits from PFT posterior file.
  segment_traits <- list(list())

  config <- PEcAn.SIPNET::write.config.SIPNET(
    defaults = settings[["pfts"]],
    trait.values = segment_traits,
    settings = segment_settings,
    run.id = segment_id
  )

  runs <- PEcAn.workflow::start_model_runs(segment_settings, write = FALSE)
}

# TODO: Post processing. Combine all the segments together and return output in
# PEcAn standard.
