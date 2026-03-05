############ Retrives soil data from gssurgo
#' This function queries the gSSURGO database for a series of map unit keys
#'
#' @param mukeys map unit key from gssurgo
#' @param fields a character vector of the fields to be extracted. See details and the default argument to find out how to define fields.
#'
#' @return a dataframe with soil properties.
#'
#' @md
#' @details 
#' This function queries the NRCS gSSURGO database using map unit keys (mukeys).  
#'
#' * **Available tables**: `mapunit`, `component`, `muaggatt`, `chorizon`, and `chfrags`.  
#' * **Field definitions**: Fields must be specified with their associated table name.  
#'   For example, total sand content is stored in the `chorizon` table and must be
#'   requested as `chorizon.sandtotal_(r|l|h)`, where:
#'   - `r` = representative value  
#'   - `l` = low value  
#'   - `h` = high value  
#'
#' **Commonly queried fields and units** (see NRCS gSSURGO ["Tables and Columns Report"](https://www.nrcs.usda.gov/sites/default/files/2022-08/SSURGO-Metadata-Tables-and-Columns-Report.pdf) 
#' for full list):
#'
#' | Field                  | Description                               | Units        |
#' |------------------------|-------------------------------------------|--------------|
#' | `chorizon.cec7_r`      | Cation exchange capacity at pH 7          | cmol(+)/kg   |
#' | `chorizon.sandtotal_r` | Total sand (<2 mm fraction)               | %            |
#' | `chorizon.silttotal_r` | Total silt (<2 mm fraction)               | %            |
#' | `chorizon.claytotal_r` | Total clay (<0.002 mm fraction)           | %            |
#' | `chorizon.om_r`        | Organic matter (<2 mm soil)               | %            |
#' | `chorizon.hzdept_r`    | Horizon top depth                         | cm           |
#' | `chfrags.fragvol_r`    | Rock fragments                            | % (by volume)|
#' | `chorizon.dbthirdbar_r`| Bulk density at field capacity            | g/cm³        |
#' | `chorizon.ph1to1h2o_r` | Soil pH (1:1 H2O)                         | pH (unitless)|
#' | `chorizon.cokey`       | Component key (identifier)                | —            |
#' | `chorizon.chkey`       | Horizon key (identifier)                  | —            |
#'
#' **API stability:** The NRCS occasionally modifies the API schema. If queries fail,
#'   adjustments may be required here to align with the updated structure. 
#'
#' Full documentation of available tables and their relationships is provided in the
#' \href{https://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}{gSSURGO documentation}.
#' @examples
#' \dontrun{
#'  PEcAn.data.land::gSSURGO.Query(
#'    mukeys = 2747727,
#'    fields = c(
#'      "chorizon.cec7_r", "chorizon.sandtotal_r",
#'      "chorizon.silttotal_r","chorizon.claytotal_r",
#'      "chorizon.om_r","chorizon.hzdept_r","chorizon.frag3to10_r",
#'      "chorizon.dbovendry_r","chorizon.ph1to1h2o_r",
#'      "chorizon.cokey","chorizon.chkey"))
#' }
#' @author Hamze Dokohaki, Akash
#' @export
gSSURGO.Query <- function(mukeys,
                          fields = c("chorizon.sandtotal_r",
                                     "chorizon.silttotal_r",
                                     "chorizon.claytotal_r")) {

  ######### Retrieve soil

  # Avoids duplicating fields that are always included in the query
  fixed_fields <- c("mapunit.mukey", "component.cokey", "component.comppct_r")
  qry_fields <- unique(fields[!(fields %in% fixed_fields)])
  
  body <- paste('<?xml version="1.0" encoding="utf-8"?>
               <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
               <soap:Body>
               <RunQuery xmlns="http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx">
               <Query>
               SELECT ',
                 paste(c(fixed_fields, qry_fields), collapse = ", "),
                 ' from mapunit
               join muaggatt on mapunit.mukey=muaggatt.mukey
               join component on mapunit.mukey=component.mukey
               join chorizon on component.cokey=chorizon.cokey
               left join chfrags on chorizon.chkey=chfrags.chkey
               where mapunit.mukey in (', paste(mukeys,collapse = ", "),');
               </Query>
               </RunQuery>
               </soap:Body>
               </soap:Envelope>')

  if (!requireNamespace("httr", quietly = TRUE)) {
    PEcAn.logger::logger.severe(
      "Package 'httr' is required for gSSURGO queries but is not installed.",
      "Please install it with: install.packages('httr')")
  }
  out <- httr::POST(
    url = "https://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx",
    config = list(
      httr::accept("text/xml"),
      httr::accept("multipart/*"),
      httr::add_headers(
        SOAPAction = "http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery")),
    httr::content_type("text/xml; charset=utf-8"), # I expected this to belong inside `config`, but doesn't seem to work there...
    encode="multipart",
    body = body)
  httr::stop_for_status(out)
  result <- httr::content(out, "text")

  suppressWarnings(
    suppressMessages({
      xml_doc <- XML::xmlTreeParse(result)
      xmltop  <- XML::xmlRoot(xml_doc)
      tablesxml <- (xmltop[[1]]["RunQueryResponse"][[1]]["RunQueryResult"][[1]]["diffgram"][[1]]["NewDataSet"][[1]])
    })
  )
  
  #parsing the table  
  tryCatch({
    suppressMessages(
      suppressWarnings({
        tables <- XML::getNodeSet(tablesxml,"//Table")
        
        ##### All datatables below newdataset
        dfs <- purrr::map_dfr(
            tables,
            function(tbl){
              lst <- purrr::map(
                XML::xmlToList(tbl),
                function(v)ifelse(is.null(v), NA, v)) #avoid dropping empty columns

              lst[names(lst) != ".attrs"]}
          )
          dfs <- dplyr::mutate(dfs, dplyr::across(dplyr::everything(), as.numeric))
      })
    )
    
    
    return(dfs)
  },
  error=function(cond) {
    print(cond)
    return(NULL)
  })
  
}

#' Get map unit keys (mukeys) from gSSURGO using spatial filters
#'
#' Queries the NRCS gSSURGO Web Feature Service to retrieve map unit keys
#' based on spatial filters: bounding box, polygon, or point with distance.
#'
#' @param bbox Numeric vector of length 4: c(xmin, ymin, xmax, ymax) in WGS84 (EPSG:4326).
#'   Features that intersect the bounding box are returned.
#' @param polygon Polygon coordinates in WGS84. Can be:
#'   - An `sf` object with a single polygon geometry
#'   - A numeric matrix with columns x (lon) and y (lat), where the first and
#'     last points are identical (closed ring)
#'   Features that intersect the polygon are returned.
#' @param point Numeric vector of length 2: c(lon, lat) in WGS84.
#'   Must be used with `distance`.
#' @param distance Numeric. Distance in meters from the point.
#'   Must be used with `point`. Use 0 for exact point intersection.
#'
#' @return Character vector of unique map unit keys (mukeys).
#'
#' @details
#' This function uses the NRCS SDM Data Access Web Feature Service:
#' \url{https://sdmdataaccess.nrcs.usda.gov/SpatialFilterHelp.htm}
#'
#' The total extent of any spatial filter cannot exceed 10,100,000,000 square
#' meters (~3,900 square miles).
#'
#' @examples
#' \dontrun{
#' # Bounding box query
#' mukeys <- ssurgo_mukeys(bbox = c(-114.006, 32.1823, -113.806, 32.2823))
#'
#' # Point with distance (600m radius)
#' mukeys <- ssurgo_mukeys(point = c(-91.22, 38.46), distance = 600)
#'
#' # Point with zero distance (exact intersection)
#' mukeys <- ssurgo_mukeys(point = c(-91.22, 38.46), distance = 0)
#'
#' # Polygon as matrix
#' poly <- rbind(
#'   c(-88.0865046533, 37.5555143852),
#'   c(-88.0860204771, 37.5600435404),
#'   c(-88.0782858287, 37.5595392364),
#'   c(-88.0787704736, 37.5550101113),
#'   c(-88.0865046533, 37.5555143852)
#' )
#' mukeys <- ssurgo_mukeys(polygon = poly)
#'
#' # Polygon as sf object
#' poly_sf <- sf::st_polygon(list(poly))
#' mukeys <- ssurgo_mukeys(polygon = poly_sf)
#' }
#' @export
ssurgo_mukeys <- function(bbox = NULL, polygon = NULL, point = NULL, distance = NULL) {
  n_provided <- sum(c(!is.null(bbox), !is.null(polygon), !is.null(point)))

  if (n_provided == 0) {
    stop("Must provide one of: bbox, polygon, or point")
  }

  if (n_provided > 1) {
    stop("Only one of bbox, polygon, or point may be provided")
  }

  if (!is.null(point)) {
    if (length(point) != 2) {
      stop("point must be a numeric vector of length 2: c(lon, lat)")
    }
    if (is.null(distance)) {
      stop("distance is required when point is provided")
    }
    if (!is.numeric(distance) || distance < 0) {
      stop("distance must be a non-negative numeric value")
    }
  }

  if (!is.null(distance) && is.null(point)) {
    stop("distance requires point to be provided")
  }

  filter_xml <- if (!is.null(bbox)) {
    if (!is.numeric(bbox) || length(bbox) != 4) {
      stop("bbox must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax)")
    }
    xmin <- bbox[1]
    ymin <- bbox[2]
    xmax <- bbox[3]
    ymax <- bbox[4]

    if (xmin >= xmax || ymin >= ymax) {
      stop("bbox must have xmin < xmax and ymin < ymax")
    }

    paste0(
      "<Filter>",
      "<BBOX>",
      "<PropertyName>Geometry</PropertyName>",
      "<Box srsName='EPSG:4326'>",
      "<coordinates>", xmin, ",", ymin, " ", xmax, ",", ymax, "</coordinates>",
      "</Box>",
      "</BBOX>",
      "</Filter>"
    )
  } else if (!is.null(polygon)) {
    coords <- if (inherits(polygon, "sfc")) {
      if (length(polygon) != 1) {
        stop("polygon (sfc) must contain exactly one geometry")
      }
      geom <- polygon[[1]]
      if (inherits(geom, "POLYGON")) {
        as.vector(t(geom))
      } else {
        stop("sfc object must contain a POLYGON geometry")
      }
    } else if (inherits(polygon, "sfg")) {
      if (inherits(polygon, "POLYGON")) {
        as.vector(t(polygon))
      } else {
        stop("sfg object must be a POLYGON")
      }
    } else if (inherits(polygon, "sf")) {
      if (nrow(polygon) != 1) {
        stop("polygon (sf) must contain exactly one feature")
      }
      geom <- sf::st_geometry(polygon)[[1]]
      if (inherits(geom, "POLYGON")) {
        as.vector(t(geom))
      } else {
        stop("sf object must contain a POLYGON geometry")
      }
    } else if (is.matrix(polygon) || is.data.frame(polygon)) {
      if (ncol(polygon) != 2) {
        stop("polygon matrix must have 2 columns: x (lon) and y (lat)")
      }
      as.vector(t(as.matrix(polygon)))
    } else {
      stop("polygon must be an sf/sfc object or a matrix/data.frame with coordinates")
    }

    coords_str <- paste(coords, collapse = " ")

    paste0(
      "<Filter>",
      "<Intersect>",
      "<PropertyName>Geometry</PropertyName>",
      "<gml:Polygon>",
      "<gml:outerBoundaryIs>",
      "<gml:LinearRing>",
      "<gml:coordinates>", coords_str, "</gml:coordinates>",
      "</gml:LinearRing>",
      "</gml:outerBoundaryIs>",
      "</gml:Polygon>",
      "</Intersect>",
      "</Filter>"
    )
  } else if (!is.null(point)) {
    lon <- point[1]
    lat <- point[2]

    paste0(
      "<Filter>",
      "<DWithin>",
      "<PropertyName>Geometry</PropertyName>",
      "<gml:Point>",
      "<gml:coordinates>", lon, ",", lat, "</gml:coordinates>",
      "</gml:Point>",
      "<Distance units=\"m\">", distance, "</Distance>",
      "</DWithin>",
      "</Filter>"
    )
  }

  base_url <- "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMWGS84Geographic.wfs"

  if (!is.null(bbox)) {
    query <- list(
      SERVICE = "WFS",
      VERSION = "1.1.0",
      REQUEST = "GetFeature",
      TYPENAME = "MapunitPoly",
      BBOX = paste(bbox, collapse = ","),
      OUTPUTFORMAT = "XMLMukeyList"
    )
    resp <- httr2::request(base_url) |>
      httr2::req_url_query(!!!query) |>
      httr2::req_perform()
  } else {
    query <- list(
      SERVICE = "WFS",
      VERSION = "1.1.0",
      REQUEST = "GetFeature",
      TYPENAME = "MapunitPoly",
      FILTER = filter_xml,
      OUTPUTFORMAT = "XMLMukeyList"
    )
    resp <- httr2::request(base_url) |>
      httr2::req_url_query(!!!query) |>
      httr2::req_perform()
  }

  httr2::resp_check_status(resp)

  resp_text <- httr2::resp_body_string(resp)

  resp_xml <- XML::xmlParse(resp_text)

  mukey_nodes <- XML::getNodeSet(resp_xml, "//MapUnitKeyList")

  if (length(mukey_nodes) == 0) {
    return(character(0))
  }

  mukey_str <- XML::xmlValue(mukey_nodes[[1]])

  if (is.null(mukey_str) || nchar(trimws(mukey_str)) == 0) {
    return(character(0))
  }

  mukeys <- unique(strsplit(trimws(mukey_str), ",")[[1]])

  mukeys
}
