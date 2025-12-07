#' Generate model-specific run configuration files for one or more PEcAn runs
#'
#' @param settings a PEcAn Settings or MultiSettings object
#' @param overwrite logical: Replace config files if they already exist?
#' @param input_design the input indices for samples (DEPRECATED - use NULL)
#' @param ens_input_design Input design matrix for ensemble (internal use)
#' @param sa_input_design Input design matrix for SA (internal use)
#' @return A modified settings object, invisibly
#' @importFrom dplyr %>%
#' @importFrom rlang %||%
#' @export


runModule.run.write.configs <- function(settings,
                                        overwrite = TRUE,
                                        input_design = NULL,
                                        ens_input_design = NULL,
                                        sa_input_design = NULL) {
  if (PEcAn.settings::is.MultiSettings(settings)) {
    if (overwrite && file.exists(file.path(settings$rundir, "runs.txt"))) {
      PEcAn.logger::logger.warn("Existing runs.txt file will be removed.")
      unlink(file.path(settings$rundir, "runs.txt"))
    }
    if (is.null(input_design) && "ensemble" %in% names(settings)) {
      ensemble_size <- settings$ensemble$size %||% 1
      design_result <- PEcAn.uncertainty::generate_joint_ensemble_design(
        settings = settings[1],
        ensemble_size = ensemble_size
      )
      ens_input_design <- design_result$X
    } else if (!is.null(input_design)) {
      ens_input_design <- input_design
    }
    
    sa_input_design <- NULL
    
    if ("sensitivity.analysis" %in% names(settings)) {
      # Load samples to determine SA run requirements
      samples.file <- file.path(settings$outdir, "samples.Rdata")
      load(samples.file)
      
      # Calculate total SA runs: 1 (median) + sum(quantiles per trait per pft)
      num_sa_runs <- 1  # Start with median run
      
      for (pft_name in names(sa.samples)) {
        if (pft_name == "env") next
        
        n_traits <- ncol(sa.samples[[pft_name]])
        quantile_names <- rownames(sa.samples[[pft_name]])
        n_quantiles <- sum(quantile_names != "50")  # Exclude median quantile
        
        num_sa_runs <- num_sa_runs + (n_traits * n_quantiles)
      }

      # Generate SA-specific input design
      design_result_sa <- PEcAn.uncertainty::generate_joint_ensemble_design(
        settings = settings[1],
        ensemble_size = num_sa_runs
      )
      sa_input_design <- design_result_sa$X
    }
    
    return(PEcAn.settings::papply(settings,
                                  runModule.run.write.configs,
                                  overwrite = FALSE,
                                  input_design = NULL,
                                  ens_input_design = ens_input_design,
                                  sa_input_design = sa_input_design))
  } else if (PEcAn.settings::is.Settings(settings)) {
    # double check making sure we have method for parameter sampling
    if (is.null(settings$ensemble$samplingspace$parameters$method)) {
      settings$ensemble$samplingspace$parameters$method <- "uniform"
    }
    if (is.null(input_design) && "ensemble" %in% names(settings)) {
      ensemble_size <- settings$ensemble$size %||% 1
      design_result <- PEcAn.uncertainty::generate_joint_ensemble_design(
        settings = settings,
        ensemble_size = ensemble_size
      )
      ens_input_design <- design_result$X
    } else if (!is.null(input_design)) {
      ens_input_design <- input_design
    }
    
    sa_input_design <- NULL
    
    if ("sensitivity.analysis" %in% names(settings)) {
      # Load samples to determine SA run requirements
      samples.file <- file.path(settings$outdir, "samples.Rdata")
      load(samples.file)
      
      # Calculate total SA runs: 1 (median) + sum(quantiles per trait per pft)
      num_sa_runs <- 1  # Start with median run
      
      for (pft_name in names(sa.samples)) {
        if (pft_name == "env") next
        
        n_traits <- ncol(sa.samples[[pft_name]])
        quantile_names <- rownames(sa.samples[[pft_name]])
        n_quantiles <- sum(quantile_names != "50")  # Exclude median quantile
        
        num_sa_runs <- num_sa_runs + (n_traits * n_quantiles)
      }

      # Generate SA-specific input design
      design_result_sa <- PEcAn.uncertainty::generate_joint_ensemble_design(
        settings = settings,
        ensemble_size = num_sa_runs
      )
      sa_input_design <- design_result_sa$X
    }
    
    ensemble_size <- nrow(ens_input_design)


    # check to see if there are posterior.files tags under pft
    posterior.files <- settings$pfts %>%
      purrr::map_chr("posterior.files", .default = NA_character_)

    return(PEcAn.workflow::run.write.configs(
      settings = settings,
      ensemble.size = ensemble_size,
      write = isTRUE(settings$database$bety$write), # treat null as FALSE
      posterior.files = posterior.files,
      overwrite = overwrite,
      input_design_ens = ens_input_design,
      input_design_sa = sa_input_design
    ))
  } else {
    stop("runModule.run.write.configs only works with Settings or MultiSettings")
  }
}