#' Extract and validate site information from settings or CSV file
#'
#' @param input Settings object, path to settings XML, or path to CSV file. 
#'        For settings objects, both single sites and MultiSettings are supported.
#' @param validate Logical. If TRUE (default), performs strict validation of coordinates.
#'        When FALSE, skips coordinate validation checks.
#' @param verbose Logical. If TRUE, prints progress messages (default: FALSE).
#'
#' @return A data frame containing site information with columns:
#' \describe{
#'   \item{site_id}{Numeric site identifier}
#'   \item{site_name}{Character site name (defaults to site_id if not provided)}
#'   \item{lat}{Numeric latitude in decimal degrees}
#'   \item{lon}{Numeric longitude in decimal degrees}
#'   \item{str_id}{Character version of site_id for display purposes}
#' }
#'
#' @export
get.site.info <- function(input, validate = TRUE, verbose = FALSE) {
  
  # Process input and collect sites in a list
  if (inherits(input, "MultiSettings")) {  
    sites_list <- purrr::map(input, ~.x$run$site)
  } else if (is.list(input) && !is.null(input$run)) {
    if (verbose) PEcAn.logger::logger.debug("Processing settings object")
    sites_list <- if (is.list(input$run) && length(input$run) > 1) {
      # Vectorized runs in single settings
        purrr::map(input$run, ~.x$site)
    } else {
      # Single run
        list(input$run$site)
    }
  } else if (is.character(input)) {
    if (!file.exists(input)) PEcAn.logger::logger.severe("File not found:", input)
    
    if (grepl("\\.xml$", input, ignore.case = TRUE)) {
      if (verbose) PEcAn.logger::logger.debug("Processing XML file:", input)
      settings <- PEcAn.settings::read.settings(input)
      # Recursive call to handle the loaded settings
      return(get.site.info(settings, validate = validate, verbose = verbose))
    } else if (grepl("\\.csv$", input, ignore.case = TRUE)) {
        if (verbose) PEcAn.logger::logger.debug("Processing CSV file:", input)
        csv_data <- utils::read.csv(input, stringsAsFactors = FALSE)
        required_cols <- c("site_id", "lat", "lon")
        if (!all(required_cols %in% colnames(csv_data))) {
          PEcAn.logger::logger.severe("Missing required columns:", setdiff(required_cols, colnames(csv_data)))
        }
        
        sites_list <- lapply(seq_len(nrow(csv_data)), function(i) {
          list(
            id = csv_data$site_id[i],
            name = if("site_name" %in% colnames(csv_data)) csv_data$site_name[i] else NULL,
            lat = csv_data$lat[i],
            lon = csv_data$lon[i]
          )
        })
    } else {
      PEcAn.logger::logger.severe("File must be XML or CSV:", input)
    }
  } else {
    PEcAn.logger::logger.severe("Input must be a settings object or file path")
  }
  
  # Unified processing for all site types
  site_info <- sites_list %>% 
    purrr::map_dfr(function(site) {
      site_id <- as.numeric(site$id)
      lat <- as.numeric(site$lat)
      lon <- as.numeric(site$lon)
      
      # Validation  
      if (validate) {
        if (!is.numeric(lat) || lat < -90 || lat > 90) {
          PEcAn.logger::logger.severe(sprintf("Invalid latitude (%s) for site: %s", lat, site_id))
        }
        if (!is.numeric(lon) || lon < -180 || lon > 180) {
          PEcAn.logger::logger.severe(sprintf("Invalid longitude (%s) for site: %s", lon, site_id))
        }
      }
      
      str_id <- if (isTRUE(site_id > 1e9)) {
        paste0(site_id %/% 1e+09, "-", site_id %% 1e+09)
      } else {
        as.character(site_id)
      }

      # Return standardized data frame row
      data.frame(
        site_id = as.integer(site_id),
        site_name = if(is.null(site$name)) as.character(site_id) else site$name,
        lat = lat,
        lon = lon,
        str_id = str_id,
        stringsAsFactors = FALSE
      )
    })
  
  return(site_info)
}
