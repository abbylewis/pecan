#' Converts a met CF file to a RothC specific input file.
#'
#' The input files are called <in.path>/<in.prefix>.YYYY.cf
#'
#' @param in.path path on disk where CF file lives
#' @param in.prefix prefix for each file
#' @param outfolder location where model specific output is written.
#' @param start_date,end_date When to start and end output.
#'  Specify as exact dates, but output will be padded to whole months.
#' @param overwrite logical: replace output files if they already exist?
#' @return OK if everything was succesful.
#' @export
#' @author Chris Black
met2model.RothC <- function(in.path,
                            in.prefix,
                            outfolder,
                            start_date,
                            end_date,
                            overwrite = FALSE) {


  PEcAn.logger::logger.info("START met2model.RothC")
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")

  # TODO pull file detection out to a helper, we are not a pasta factory
  if (grepl("\\.nc$", in.prefix)) {
    # assume it's the full filename rather than a prefix
    name_pattern <- in.prefix
  } else {
    name_pattern <- paste0(in.prefix, ".*", "\\.nc$")
  }

  nc_files <- list.files(in.path, pattern = name_pattern)

  if (length(nc_files) == 0) {
    PEcAn.logger::logger.severe(
      paste0("No files found matching ", in.prefix, "; cannot process data.")
    )
  }

  out.file <- paste(
    in.prefix,
    strptime(start_date, "%Y-%m"),
    strptime(end_date, "%Y-%m"),
    "dat",
    sep = "."
  )

  out.file.full <- file.path(outfolder, out.file)

  results <- data.frame(file = out.file.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/tab-separated-values",
                        formatname = "RothC.dat",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = out.file,
                        stringsAsFactors = FALSE)
  PEcAn.logger::logger.info("internal results")
  PEcAn.logger::logger.info(results)

  if (file.exists(out.file.full) && !overwrite) {
    PEcAn.logger::logger.debug(
      "File '", out.file.full, "' already exists, skipping to next file."
    )
    return(invisible(results))
  }

  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }



  # construct vector of input filenames
  # (specifically including multiple years if dates include that)
  met <-  nc_files |>
    lapply(
      read_nc,
      varnames = c("air_temperature", "precipitation_flux", "specific_humidity")
    ) |>
    do.call(what = "rbind")

  met$year <- lubridate::year(met$timestamp)
  met$month <- lubridate::month(met$timestamp)

  met$Tmp <- this_years_vals$air_temperature |>
    PEcAn.utils::ud_convert("K", "degC")
  met$Rain <- 0# TODO... sum up to convert from flux to accumulation, right?
  met$Evap <- 0# TODO... how to convert Qair to pan evaporation?


  met_monthly <- aggregate(met, Tmp ~ year + month, mean)
  met_monthly$Rain <- tapply(met, Rain ~ year + month, sum) # careful, assumes it was converted from flux earlier
  met_monthly$Evap <- tapply(met, Evap ~ year + month, sum)




}


# slurp named vars from one PEcAn nc into a dataframe with timestamp
#
# TODO could read other dimensions if present too, but consider if worth it --
# maybe this function is better for files where only the time dimension varies
#
# if vars = NULL, read all of them
read_nc <- function(ncfile, varnames = NULL) {

  nc <- ncdf4::nc_open(ncfile)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  timestamps <- PEcAn.utils::cf2datetime(
    nc$dim$time$vals,
    nc$dim$time$units
  )

  if (is.null(varnames)) {
    varnames <- names(nc$var)
  }

  var_values <- lapply(
    varnames,
    ncdf4::ncvar_get,
    nc = nc
  )

  # todo handle this case (multi-loc files?)
  stopifnot(all(sapply(var_values, length) == length(timestamps)))

  var_values |>
    setNames(varnames) |>
    as.data.frame() |>
    transform(timestamp = timestamps)
}
