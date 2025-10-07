##' Writes a RothC config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for MODEL for given run
##' @export
##' @author Rob Kooper
write.config.RothC <- function(defaults, trait.values, settings, run.id) {

  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)

  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.RothC"), n = -1)
  }

  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }

  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }

  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)

  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", settings$run$site$met, jobsh)

  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)

  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)

  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))

  #-----------------------------------------------------------------------
  ### Edit a templated config file for runs
  if (!is.null(settings$model$config) && file.exists(settings$model$config)) {
    config.text <- readLines(con = settings$model$config, n = -1)
  } else {
    filename <- system.file(settings$model$config, package = "PEcAn.MODEL")
    if (filename == "") {
      if (!is.null(settings$model$revision)) {
        filename <- system.file(paste0("config.", settings$model$revision), package = "PEcAn.MODEL")
      } else {
        model <- PEcAn.DB::db.query(paste("SELECT * FROM models WHERE id =", settings$model$id), params = settings$database$bety)
        filename <- system.file(paste0("config.r", model$revision), package = "PEcAn.MODEL")
      }
    }
    if (filename == "") {
      PEcAn.logger::logger.severe("Could not find config template")
    }
    PEcAn.logger::logger.info("Using", filename, "as template")
    config.text <- readLines(con = filename, n = -1)
  }

  config.text <- gsub("@SITE_LAT@", settings$run$site$lat, config.text)
  config.text <- gsub("@SITE_LON@", settings$run$site$lon, config.text)
  config.text <- gsub("@SITE_MET@", settings$run$inputs$met$path, config.text)
  config.text <- gsub("@MET_START@", settings$run$site$met.start, config.text)
  config.text <- gsub("@MET_END@", settings$run$site$met.end, config.text)
  config.text <- gsub("@START_MONTH@", format(settings$run$start.date, "%m"), config.text)
  config.text <- gsub("@START_DAY@", format(settings$run$start.date, "%d"), config.text)
  config.text <- gsub("@START_YEAR@", format(settings$run$start.date, "%Y"), config.text)
  config.text <- gsub("@END_MONTH@", format(settings$run$end.date, "%m"), config.text)
  config.text <- gsub("@END_DAY@", format(settings$run$end.date, "%d"), config.text)
  config.text <- gsub("@END_YEAR@", format(settings$run$end.date, "%Y"), config.text)
  config.text <- gsub("@OUTDIR@", settings$host$outdir, config.text)
  config.text <- gsub("@ENSNAME@", run.id, config.text)
  config.text <- gsub("@OUTFILE@", paste0("out", run.id), config.text)

# TODO make these editable -- hard-coding for MVP
# OPT_RMMOIST: soil water parameterization.
#   "1: Standard RothC soil water parameters"
#   "2: Van Genuchten soil properties and soil is allowed to be drier (ie hygroscopic / capillary water, -1000bar)"
#   "3: Van Genuchten soil properties, but uses the Standard RothC soil water function"
config.text <- gsub("@OPT_RMMOIST@", "1", config.text)
# Bare SMD: wilting point configuration
#   "1: Standard RothC bareSMD"
#   "2: bareSMD is set to wilting point -15bar (could be better for dry soils)"
config.text <- gsub("@OPT_SDDBARE@", "1", config.text)

# Soil parameters -- TODO read from run$inputs$soil_physics
# clay_pct, depth_cm, iom_tC_ha, nsteps, silt_pct, bulkdens_g_m3, org_C_pct, min_RM_moist
config.text <- gsub("@SOIL_PARAMS@", "23.4  23.0   3.0041      840    58.6      1.27  0.94   0.2", config.text)

# Climate data + management inputs
# TODO all managements hardcoded for MVP
met_path <- inputs$met$path %||% settings$run$inputs$met$path
met_in <- read.table(met_path, header = FALSE)
zros <- rep(0, nrow(met_in))
inputs <- data.frame(
  C_inp_tC_ha = zros,
  FYM_tC_ha =  zros,
  PC =  zros,
  PL_DPM_f =  zros,
  PL_RPM_f =  zros,
  OA_DPM_f =  zros,
  OA_RPM_f =  zros,
  OA_BIO_f =  zros,
  OA_HUM_f =  zros
)

input_rows <- met_in |>
  bind_cols(inputs) |>
  dplyr::select(all_of(
    "year", "month",
    "modern_pct",
    "Tmp_C", "Rain_mm", "Evap_mm",
    "C_inp_tC_ha", "FYM_tC_ha", "PC",
    "PL_DPM_f", "PL_RPM_f",
    "OA_DPM_f", "OA_RPM_f", "OA_BIO_f", "OA_HUM_f"
  )) |>
  # Kinda ugly: Convert to one string to cram it into the template via gsub
  format() |>
  apply(1, paste, collapse = " ") |>
  paste(collapse = "\n")

  config.text <- gsub("@CLIM_DATA@", input_rows, config.text)

  config.file.name <- "RothC_input.dat"
  writeLines(config.text, con = file.path(outdir, config.file.name))
} # write.config.MODEL
