##' Writes a PEPRMT config file.
##'
##' Requires a pft xml object, a list of trait values for a single PEPRMT run,
##' and the name of the file to create
##'
##' @name write.config.PEPRMT
##' @title Write PEPRMT configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for PEPRMT for given run
##' @export
##' @author Rob Kooper, edited by Abby Lewis
##-------------------------------------------------------------------------------------------------#
write.config.PEPRMT <- function(defaults, trait.values, settings, run.id) {

  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.PEPRMT"), n = -1)
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
  # jobsh <- gsub("@SITE_MET@", settings$run$site$met, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  jobsh <- gsub("@DELETE_RAW@", as.logical(settings$model$delete.raw %||% FALSE), jobsh)
  
  #-----------------------------------------------------------------------
  ### Edit a templated config file for runs
  # if (!is.null(settings$model$config) && file.exists(settings$model$config)) {
  #   config.text <- readLines(con = settings$model$config, n = -1)
  # } else {
  #   filename <- system.file(settings$model$config, package = "PEcAn.PEPRMT")
  #   if (filename == "") {
  #     if (!is.null(settings$model$revision)) {
  #       filename <- system.file(paste0("config.", settings$model$revision), package = "PEcAn.PEPRMT")
  #     }
  #   }
  #   if (filename == "") {
  #     PEcAn.logger::logger.severe("Could not find config template")
  #   }
  #   PEcAn.logger::logger.info("Using", filename, "as template")
  #   config.text <- readLines(con = filename, n = -1)
  # }
  
  # config.text <- gsub("@SITE_LAT@", settings$run$site$lat, config.text)
  # config.text <- gsub("@SITE_LON@", settings$run$site$lon, config.text)
  # config.text <- gsub("@SITE_MET@", settings$run$inputs$met$path, config.text)
  # config.text <- gsub("@MET_START@", settings$run$site$met.start, config.text)
  # config.text <- gsub("@MET_END@", settings$run$site$met.end, config.text)
  # config.text <- gsub("@START_MONTH@", format(settings$run$start.date, "%m"), config.text)
  # config.text <- gsub("@START_DAY@", format(settings$run$start.date, "%d"), config.text)
  # config.text <- gsub("@START_YEAR@", format(settings$run$start.date, "%Y"), config.text)
  # config.text <- gsub("@END_MONTH@", format(settings$run$end.date, "%m"), config.text)
  # config.text <- gsub("@END_DAY@", format(settings$run$end.date, "%d"), config.text)
  # config.text <- gsub("@END_YEAR@", format(settings$run$end.date, "%Y"), config.text)
  # config.text <- gsub("@OUTDIR@", settings$host$outdir, config.text)
  # config.text <- gsub("@ENSNAME@", run.id, config.text)
  # config.text <- gsub("@OUTFILE@", paste0("out", run.id), config.text)
  
  #-----------------------------------------------------------------------
  # config.file.name <- paste0("CONFIG.", run.id, ".txt")
  # writeLines(config.text, con = paste(outdir, config.file.name, sep = "")
  
  # Load met data
  loaded_met <- PEcAn.data.atmosphere::download.ERA5_cds(outfolder = rundir, 
                                           start_date = settings$run$start.date,
                                           end_date = settings$run$end.date,
                                           extent = c(settings$run$site$lon,
                                                      settings$run$site$lon,
                                                      settings$run$site$lat, 
                                                      settings$run$site$lat),
                                           user = "abigail.sl.lewis@gmail.com",
                                           key = Sys.getenv("key"),
                                           variables = c("2m_temperature",
                                                         "surface_pressure")
                                           )
  
  nc_path <- loaded_met[[1]]$file
  in.path <- dirname(nc_path)
  in.prefix <- "ERA5_"
  opened <- ncdf4::nc_open(nc_path)
  result <- met2model.PEPRMT(in.path, in.prefix, outfolder = rundir, 
                             start_date = settings$run$start.date, 
                             end_date = settings$run$end.date)
  final.nc.files <- extract.nc.ERA5(slat = settings$run$site$lat, 
                                    slon = settings$run$site$lon, 
                                    in.path, 
                                    start_date = settings$run$start.date, 
                                    end_date = settings$run$end.date, 
                                    outfolder = in.path, 
                                    in.prefix = "ERA5_", 
                                    newsite = settings$run$site$id)
  AmeriFlux_met_ensemble(site_id = settings$run$site$id,
                         start_date = settings$run$start.date,
                         end_date = settings$run$end.date, 
                         outfolder = in.path,
                         ameriflux_username = "pattyoikawa", 
                         ameriflux_useremail = "patty.oikawa@csueastbay.edu",
                         overwrite = FALSE, 
                         verbose = T, 
                         format = NULL,
                         n_ens = 10, 
                         w_len = 30,
                         era5_user = "abigail.sl.lewis@gmail.com",
                         era5_key = Sys.getenv("key"),
                         threshold = 0.5,
                         dirs = NULL)
  
  # yes, this will be replaced with real params once demo is working
  run_data <- PEPRMT::example_data |>
    dplyr::filter(.data$site == settings$run$site$id)
  
  utils::write.csv(run_data, file.path(rundir, "run_data.csv"), row.names = FALSE)
  writeLines(jobsh, con = file.path(rundir, "job.sh"))
  Sys.chmod(file.path(rundir, "job.sh"))
} # write.config.PEPRMT
