#' Generate One-At-a-Time (OAT) input design for sensitivity analysis
#'
#' Creates an input design matrix for sensitivity analysis where non-parameter
#' inputs (met, IC, soil, etc.) are held constant while parameters vary
#' one-at-a-time across quantiles. This differs from ensemble design where
#' all inputs vary together.
#'
#' @param settings PEcAn settings object
#' @param sa_samples Optional. Pre-loaded SA samples from samples.Rdata.
#'   If NULL, loads from settings$outdir/samples.Rdata
#'
#' @return list with component X: a data.frame with columns for each input type
#'   and one row per SA run. Non-parameter columns are all 1 (constant).
#'
#' @details For sensitivity analysis, we must isolate the effect of each 
#'   parameter by holding all other inputs constant. The param column contains
#'   sequential indices (1, 2, 3, ...) matching the SA run order in 
#'   write.sa.configs. All other columns (met, ic, soil, etc.) are set to 1,
#'   meaning the first input file is always used.
#'
#' @examples
#' \dontrun{
#' # Generate SA design for a multi-site run
#' sa_design <- generate_OAT_SA_design(settings)
#' 
#' # View the design matrix
#' print(sa_design$X)
#' #   param met ic soil
#' # 1     1   1  1    1   # Median run
#' # 2     2   1  1    1   # trait1 @ q=2.3%
#' # 3     3   1  1    1   # trait1 @ q=15.9%
#' # 4     4   1  1    1   # trait1 @ q=84.1%
#' # ...
#' 
#' # All non-param columns are constant (1)
#' }
#' @export
#' @author Akash B V
#' @importFrom rlang %||%
generate_OAT_SA_design <- function(settings, sa_samples = NULL) {
  
  # load SA samples if not provided
  if (is.null(sa_samples)) {
    samples_file <- file.path(settings$outdir, "samples.Rdata")
    
    # generate samples if they don't exist (safety fallback)
    if (!file.exists(samples_file)) {
      posterior.files <- settings$pfts %>%
        purrr::map_chr("posterior.files", .default = NA_character_)
      ens.sample.method <- settings$ensemble$samplingspace$parameters$method %||% "uniform"
      
      PEcAn.uncertainty::get.parameter.samples(
        settings,
        ensemble.size = 1,  # SA doesn't need ensemble samples
        posterior.files,
        ens.sample.method
      )
    }
    
    samples_env <- new.env()
    load(samples_file, envir = samples_env)
    sa_samples <- samples_env$sa.samples
    
    if (is.null(sa_samples)) {
      PEcAn.logger::logger.severe(
        "sa.samples not found in samples.Rdata.",
        "Ensure sensitivity.analysis is configured in settings."
      )
    }
  }
  
  # calculate total number of SA runs
  # 1 median + (traits * non-median quantiles) per PFT
  MEDIAN <- "50"
  num_sa_runs <- 1 # start with median run
  
  for (pft_name in names(sa_samples)) {
    if (pft_name == "env") next
    
    pft_samples <- sa_samples[[pft_name]]
    n_traits <- ncol(pft_samples)
    quantile_names <- rownames(pft_samples)
    n_non_median <- sum(quantile_names != MEDIAN)
    
    # add runs for this pft: (traits) * (non-median quantiles)
    num_sa_runs <- num_sa_runs + (n_traits * n_non_median) # num_sa_runs += n_traits * n_non_median
  }
  
  # get input types from samplingspace
  samp <- settings$ensemble$samplingspace
  input_types <- names(samp)
  input_types[input_types == "parameters"] <- "param"
  
  if (!"param" %in% input_types) {
    input_types <- c("param", input_types)
  }
  
  # build design matrix
  # key difference from ensemble design:
  # - ensemble: all columns get random/quasi-random indices
  # - SA (OAT): param column = sequential index, ALL other columns = 1
  #
  # the "1" means: use the FIRST (and only) input file for that type.
  # this ensures all SA runs use the SAME met, same ic, etc.

  design_list <- list()
  
  for (input_type in input_types) {
    if (input_type == "param") {
      # sequential indices map to SA run order
      #   1 = median run
      #   2 = first (pft, trait, quantile) combination
      #   3 = second (pft, trait, quantile) combination
      #   ...
      design_list[[input_type]] <- seq_len(num_sa_runs)
    } else {
      # all other inputs constant(always use first input file)
      design_list[[input_type]] <- rep(1L, num_sa_runs)
    }
  }
  
  design_matrix <- data.frame(design_list)

  return(list(X = design_matrix))
}