#' Retrieve trait data and priors for one PFT from BETYdb
#'
#' Core computation extracted from \code{\link{get.trait.data.pft}}.
#' Queries the database for trait observations and prior distributions,
#' returning them as R objects with no file I/O of any kind.
#'
#' The wrapper \code{\link{get.trait.data.pft}} handles directory creation,
#' caching, CSV output, and BETYdb registration via \code{dbfile.insert}.
#' This function handles only the query. Choosing between them is the
#' provenance opt-in: calling the wrapper saves artifacts to disk; calling
#' this function never does.
#'
#' This follows the pattern established by \code{meta_analysis_standalone}
#' for the meta-analysis step and \code{get_parameter_samples} (in PEcAn.DB) for
#' parameter sampling — each is a computation core that can be tested in
#' isolation without a filesystem or a \code{settings} object.
#'
#' @param pft_name character. PFT name as stored in BETYdb.
#' @param modeltype character or NULL. Disambiguates PFTs that share a name
#'   across model types (e.g. \code{"SIPNET"}, \code{"ED2"}).
#' @param dbcon database connection from \code{\link[PEcAn.DB]{db.open}}.
#' @param trait_names character vector of trait names to retrieve.
#' @param constants named list from \code{pft$constants} in the settings.
#'   Traits named here are excluded from the returned priors because their
#'   values are fixed rather than sampled by the meta-analysis.
#'
#' @return Named list with three elements:
#' \describe{
#'   \item{\code{trait_data}}{Named list of data frames, one per trait that
#'     has observations. Column structure matches what
#'     \code{meta_analysis_standalone} expects. Traits with no observations
#'     are omitted from the list.}
#'   \item{\code{prior_distns}}{Data frame with columns \code{distn},
#'     \code{parama}, \code{paramb}, \code{n}; rows named by trait. Traits
#'     listed in \code{constants} are excluded.}
#'   \item{\code{pft_info}}{List with \code{name}, \code{pft_id},
#'     \code{pft_type}, and \code{posteriorid}. \code{posteriorid} is always
#'     \code{NULL} — the wrapper sets it after registering outputs in BETYdb.}
#' }
#'
#' @seealso \code{\link{get.trait.data.pft}} for the backward-compatible
#'   wrapper that handles provenance and caching.
#'   \code{meta_analysis_standalone} (in PEcAn.MA) for the analogous
#'   function in the meta-analysis step.
#'   \code{get_parameter_samples} (in PEcAn.DB) for the analogous function in the
#'   parameter sampling step.
#'
#' @examples
#' \dontrun{
#' dbcon <- PEcAn.DB::db.open(list(
#'   host = "localhost", user = "bety",
#'   password = "bety",  dbname = "bety"
#' ))
#' result <- get_trait_data_pft(
#'   pft_name    = "temperate.deciduous",
#'   modeltype   = "SIPNET",
#'   dbcon       = dbcon,
#'   trait_names = c("SLA", "Vcmax", "leaf_respiration_rate_m2")
#' )
#' str(result$trait_data)
#' str(result$prior_distns)
#' PEcAn.DB::db.close(dbcon)
#' }
#'
#' @author David LeBauer, Shawn Serbin, Alexey Shiklomanov, Om Kapale
#' @export
get_trait_data_pft <- function(pft_name,
                               modeltype,
                               dbcon,
                               trait_names,
                               constants = list()) {

  # Validate the cheap arguments before making any database calls
  if (!is.character(pft_name) || length(pft_name) != 1L) {
    PEcAn.logger::logger.severe("'pft_name' must be a single character string")
  }
  if (!is.character(trait_names) || length(trait_names) == 0L) {
    PEcAn.logger::logger.severe(
      "'trait_names' must be a non-empty character vector"
    )
  }
  if (!inherits(dbcon, "DBIConnection")) {
    PEcAn.logger::logger.severe("'dbcon' must be a database connection")
  }

  # Resolve PFT name to a single database record.
  # strict = TRUE gives a clear error when the PFT is not found rather than
  # returning an empty data frame silently.
  pft_record <- query_pfts(dbcon, pft_name, modeltype, strict = TRUE)

  if (nrow(pft_record) > 1L) {
    PEcAn.logger::logger.severe(
      "Multiple PFTs named '", pft_name, "' found in the database;",
      " pass modeltype to disambiguate."
    )
  }

  pft_id   <- pft_record[["id"]]
  pft_type <- pft_record[["pft_type"]]

  PEcAn.logger::logger.info(
    "Querying trait data for PFT '", pft_name, "' (id = ", pft_id, ")"
  )

  # Which join table holds the member IDs depends on pft_type
  ids_are_cultivars <- identical(pft_type, "cultivar")

  if (ids_are_cultivars) {
    members <- query.pft_cultivars(pft = pft_name, modeltype = modeltype,
                                   con = dbcon)
  } else {
    members <- query.pft_species(pft = pft_name, modeltype = modeltype,
                                 con = dbcon)
  }
  members <- members %>%
    dplyr::mutate_if(is.character, ~dplyr::na_if(., ""))
  member_ids <- members[["id"]]

  if (length(member_ids) == 0L) {
    PEcAn.logger::logger.info(
      "PFT '", pft_name, "' has no associated ",
      if (ids_are_cultivars) "cultivars" else "species",
      "; trait_data will be an empty list."
    )
  }

  # format() prevents integer64 from being silently coerced in the SQL query
  # (same approach used in get.trait.data())
  prior_distns <- query.priors(
    pft   = format(pft_id, scientific = FALSE),
    trstr = PEcAn.utils::vecpaste(trait_names),
    con   = dbcon
  )

  # Traits in pft$constants have fixed values and are never sampled, so they
  # should not appear in the prior distributions returned to callers
  if (length(constants) > 0L && !is.null(names(constants))) {
    constant_traits <- names(constants)
    in_constants    <- rownames(prior_distns) %in% constant_traits
    if (any(in_constants)) {
      PEcAn.logger::logger.info(
        "Excluding ", sum(in_constants), " constant trait(s) from priors: ",
        PEcAn.utils::vecpaste(rownames(prior_distns)[in_constants])
      )
      prior_distns <- prior_distns[!in_constants, , drop = FALSE]
    }
  }

  # Only query traits that have a prior — querying for traits with no prior
  # is meaningless for meta-analysis
  traits_with_priors <- rownames(prior_distns)

  if (length(member_ids) > 0L && length(traits_with_priors) > 0L) {
    trait_data <- query.traits(
      ids               = member_ids,
      priors            = traits_with_priors,
      con               = dbcon,
      ids_are_cultivars = ids_are_cultivars
    )
  } else {
    trait_data <- list()
  }

  PEcAn.logger::logger.info(
    "PFT '", pft_name, "': ",
    length(trait_data), " trait(s) with observations, ",
    nrow(prior_distns), " trait(s) with priors"
  )

  # posteriorid is NULL here — the wrapper sets it after registering the
  # output files in BETYdb via dbfile.insert()
  pft_info <- list(
    name        = pft_name,
    pft_id      = pft_id,
    pft_type    = pft_type,
    posteriorid = NULL
  )

  return(list(
    trait_data   = trait_data,
    prior_distns = prior_distns,
    pft_info     = pft_info
  ))
}