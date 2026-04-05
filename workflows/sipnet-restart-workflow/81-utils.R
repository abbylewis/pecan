#' settings for whole run with many paths per input ->
#' settings for one ensemble member with one path per input
#'
#' @param settings single-site settings object (not Multisettings)
#' @param inputs named list of input indices (one row of the sample design)
subset_paths <- function(settings, path_nums) {
  for (input in names(path_nums)) {
    if (!(input %in% names(settings$run$inputs))) {
      next
    }
    path_idx <- path_nums[[input]]
    all_paths <- settings$run$inputs[[input]]$path
    if (path_idx > length(all_paths)) {
      PEcAn.logger::logger.severe("No path at input ", sQuote(input), " index ", path_idx)
    }
    settings$run$inputs[[input]]$path <- all_paths[[path_idx]]
  }
  settings
}

crop2pft <- function(crop_code) {
  # crop_code <- c("F1", "R1", "G2", "F16")
  cls <- substr(crop_code, 1, 1)
  dplyr::case_when(
    cls == "D" ~ "temperate.deciduous",
    cls == "F" ~ "annual_crop",
    cls == "G" ~ "grass",
    cls == "P" ~ "grass",
    cls == "R" ~ "grass",
    is.na(crop_code) ~ "soil",
    TRUE ~ "UNKNOWN_PFT"
  )
}

run_sipnet_segmented <- function(
  settings,
  run_row,
  events_json_file,
  crop2pft = crop2pft
) {
  run_id <- run_row[["run_id"]]
  run_dir <- file.path(settings$rundir, run_id)
  run_modeloutdir <- file.path(settings$modeloutdir, run_id)
  run_settings <- subset_paths(settings, run_row)

  ens_samples_file <- file.path(
    run_settings$outdir,
    sprintf("ensemble.samples.%s.Rdata", run_settings$ensemble$ensemble.id)
  )
  stopifnot(file.exists(ens_samples_file))
  ensemble_samples <- PEcAn.utils::load_local(ens_samples_file)[["ens.samples"]]
  run_traits <- lapply(ensemble_samples, \(dat) dat[run_row[["param"]], ])

  # TODO: Store this in settings or something?
  crop_cycles <- PEcAn.data.land::events_to_crop_cycle_starts(events_json_file)

  # Get segments
  segments <- data.frame(
    start_date = c(as.Date(run_settings$run$start.date), crop_cycles[["date"]]),
    end_date = c(crop_cycles[["date"]] - 1, as.Date(run_settings$run$end.date)),
    crop_code = c(NA_character_, crop_cycles[["crop_code"]])
  )
  segments[["pft"]] <- crop2pft(segments[["crop_code"]])
  segments[["segment_id"]] <- sprintf("%03d", seq_len(nrow(segments)))
  segment_rootdir <- file.path(file.path(run_dir, "segments"))
  segments[["segment_dir"]] <- file.path(
    segment_rootdir,
    sprintf("segment_%s", segments[["segment_id"]])
  )

  for (isegment in seq_len(nrow(segments))) {
    segment <- segments[isegment, ]
    dstart <- segment[["start_date"]]
    dend <- segment[["end_date"]]
    segment_dir <- segment[["segment_dir"]]

    unlink(segment_dir, recursive = TRUE)
    dir.create(segment_dir, showWarnings = FALSE, recursive = TRUE)

    runid_dummy <- "1"

    segment_inputs <- PEcAn.SIPNET::split_inputs.SIPNET(
      dstart,
      dend,
      run_settings$run$inputs,
      overwrite = TRUE,
      outpath = segment_dir
    )

    # Segment-specific settings
    segment_outdir <- file.path(segment_dir, "out")
    dir.create(segment_outdir, showWarnings = FALSE, recursive = TRUE)
    segment_rundir <- file.path(segment_dir, "run")
    dir.create(segment_rundir, showWarnings = FALSE, recursive = TRUE)
    file.create(file.path(segment_rundir, "README.txt"))

    segment_rundir_withid <- file.path(segment_rundir, runid_dummy)
    dir.create(segment_rundir_withid, showWarnings = FALSE, recursive = TRUE)
    segment_outdir_withid <- file.path(segment_outdir, runid_dummy)
    dir.create(segment_outdir_withid, showWarnings = FALSE, recursive = TRUE)

    segment_settings <- run_settings
    segment_settings[["outdir"]] <- segment_outdir
    segment_settings[["modeloutdir"]] <- segment_outdir
    segment_settings[["rundir"]] <- segment_rundir
    segment_settings[[c("run", "start.date")]] <- dstart
    segment_settings[[c("run", "end.date")]] <- dend
    segment_settings[[c("run", "inputs")]] <- segment_inputs
    segment_settings
    if (is.null(segment_settings[[c("model", "options")]])) {
      segment_settings[[c("model", "options")]] <- list()
    }

    if (isegment > 1) {
      # For isegment > 1, we restart from the *previous* segment's restart.out
      segment_settings[[c("model", "options", "RESTART_IN")]] <- restart_out
    }
    # ...and now, define a new restart.out for *this* segment
    restart_out <- file.path(segment_rundir, "restart.out")
    segment_settings[[c("model", "options", "RESTART_OUT")]] <- restart_out

    segment_traits <- run_traits[[segment[["pft"]]]]

    # Write dummy runs file
    writeLines(runid_dummy, file.path(segment_rundir, "runs.txt"))

    PEcAn.SIPNET::write.config.SIPNET(
      defaults = segment_settings[["pfts"]],
      trait.values = segment_traits,
      settings = segment_settings,
      run.id = runid_dummy
    )

    PEcAn.workflow::start_model_runs(segment_settings, write = FALSE)

    PEcAn.SIPNET::model2netcdf.SIPNET(
      outdir = segment_outdir_withid,
      sitelat = segment_settings[[c("run", "site", "lat")]],
      sitelon = segment_settings[[c("run", "site", "lon")]],
      start_date = dstart,
      end_date = dend,
      revision = segment_settings[[c("model", "revision")]],
      overwrite = TRUE
    )
  }

  segment_ncfiles <- lapply(
    segments[["segment_dir"]],
    \(x) {
      list.files(
        file.path(x, "out", "1"),
        pattern = "\\d+\\.nc",
        full.names = TRUE
      )
    }
  ) |>
    do.call(what = c)

  segment_files_byyear <- split(
    segment_ncfiles,
    factor(basename(segment_ncfiles))
  )
  segment_outfiles <- file.path(run_modeloutdir, names(segment_files_byyear))
  results <- purrr::map2(
    segment_files_byyear,
    segment_outfiles,
    PEcAn.SIPNET::mergeNC
  )
  results
}
