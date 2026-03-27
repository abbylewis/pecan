#!/usr/bin/env Rscript

devtools::load_all("~/projects/pecan/sipnet-events/modules/data.land")
devtools::load_all("~/projects/pecan/sipnet-events/models/sipnet")

config <- config::get(file = "modules/data.land/inst/sipnet-restart-workflow/config.yml")

outdir_root <- config[["outdir_root"]]

binary <- config[["sipnet_binary"]]
stopifnot(file.exists(binary))

site_id <- config[["site_id"]]

events_json_file <- fs::path(outdir_root, "events.json")
events <- jsonlite::read_json(events_json_file, simplifyVector = FALSE)

dates <- purrr::

################################################################################
outdir <- fs::path(outdir_root, "segments") |> fs::dir_create()

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
