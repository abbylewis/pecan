#' Generate joint ensemble design for parameter sampling
#' Creates a joint ensemble design that maintains parameter correlations across
#' all sites in a multi-site run. This function generates sample indices that are shared across sites to ensure consistent parameter sampling.
#'
##' @param settings A PEcAn settings object containing ensemble configuration
##' @param sobol for generating inputs for sobol
##' @param ensemble_size Integer specifying the number of ensemble members
##' @return  A list containing ensemble samples and indices
##' 
##' @export

generate_joint_ensemble_design <- function(settings, ensemble_size, sobol = FALSE ) {
  
  ens.sample.method <- settings$ensemble$samplingspace$parameters$method
  design_list <- list()
  sampled_inputs <- list()
  posterior.files = rep(NA, length(settings$pfts))
  samp <- settings$ensemble$samplingspace
  parents <- lapply(samp, '[[', 'parent')
  order <- names(samp)[lapply(parents, function(tr) which(names(samp) %in% tr)) %>% unlist()]
  samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]
  
  
  # Sample parameters
  PEcAn.uncertainty::get.parameter.samples(settings, ensemble.size = ensemble_size, posterior.files, ens.sample.method)
  
  
  # Load samples from file
  samples.file <- file.path(settings$outdir, "samples.Rdata")
  samples <- new.env()
  if (file.exists(samples.file)) {
    load(samples.file, envir = samples)
    if (!is.null(samples$ensemble.samples)) {
      # Just a placeholder: extract representative trait index per ensemble member
      # You may want to flatten or select indices per trait
      design_list[["param"]] <- seq_len(ensemble_size)
    } else {
      PEcAn.logger::logger.warn("ensemble.samples not found in samples.Rdata")
    }
  } else {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required")
  }
  design_matrix<- data.frame(design_list)
  
  
  
  
 if(sobol){
    
    # extracting parameter 
    
    
    samples.file <- file.path(settings$outdir, "samples.Rdata")
    if (file.exists(samples.file)) {
      samples <- new.env()
      load(samples.file, envir = samples) ## loads ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples
      trait.samples <- samples$trait.samples
      
      
      trait_sample_indices <- design_matrix[["param"]]
      ensemble.samples <- list()
      for (pft in names(trait.samples)) {
        pft_traits <- trait.samples[[pft]]
        ensemble.samples[[pft]] <- as.data.frame(
          lapply(
            names(pft_traits),
            function(trait) pft_traits[[trait]][trait_sample_indices]
          )
        )
        names(ensemble.samples[[pft]]) <- names(pft_traits)
      }
      sa.samples <- samples$sa.samples
      runs.samples <- samples$runs.samples
      ## env.samples <- samples$env.samples
      
    } else {
      PEcAn.logger::logger.error(samples.file, "not found, this file is required by the run.write.configs function")
    }
    
  
    all_params <- ensemble.samples$temperate.deciduous.HPDA
 
    half_size <- floor(ensemble_size / 2)
    X1 <- all_params[1:half_size, ]
    X2 <- all_params[(half_size + 1):ensemble_size, ]
  
    sobol_obj <- soboljansen(model = NULL, X1 = X1, X2 = X2)
    sobol_obj
    U <- sobol_obj$X
    U
    ensemble.samples$temperate.deciduous.HPDA <-U
    all.param.samples <- list(
      trait.samples = trait.samples,
      ensemble.samples = ensemble.samples ,
      sa.samples = sa.samples,
      runs.samples = runs.samples
      # env.samples = samples$env.samples  # Uncomment if needed
    )
    
    
 
    
    
    #recreating samples 
    ensemble_size <- nrow(U) 
    
    input_design <- PEcAn.uncertainty::generate_joint_ensemble_design(settings=settings, ensemble_size = ensemble_size)
    
    
    #param for sobol
    # extracting parameter 
    
    
    samples.file <- file.path(settings$outdir, "samples.Rdata")
    if (file.exists(samples.file)) {
      samples <- new.env()
      load(samples.file, envir = samples) ## loads ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples
      trait.samples <- samples$trait.samples
      
      
      trait_sample_indices <- input_design[["param"]]
      ensemble.samples <- list()
      for (pft in names(trait.samples)) {
        pft_traits <- trait.samples[[pft]]
        ensemble.samples[[pft]] <- as.data.frame(
          lapply(
            names(pft_traits),
            function(trait) pft_traits[[trait]][trait_sample_indices]
          )
        )
        names(ensemble.samples[[pft]]) <- names(pft_traits)
      }
      sa.samples <- samples$sa.samples
      runs.samples <- samples$runs.samples
      ## env.samples <- samples$env.samples
      
    } else {
      PEcAn.logger::logger.error(samples.file, "not found, this file is required by the run.write.configs function")
    }
    ensemble.samples$temperate.deciduous.HPDA <- U
    all.param.samples <- list(
      trait.samples = trait.samples,
      ensemble.samples = ensemble.samples ,
      sa.samples = sa.samples,
      runs.samples = runs.samples
      # env.samples = samples$env.samples  # Uncomment if needed
    )
    sobol_obj
    return(list(input_design = input_design, all.param.samples = all.param.samples, sobol_obj = sobol_obj))
    
    
 }
  
  
  
  
  
  ens.sample.method <- settings$ensemble$samplingspace$parameters$method
  design_list <- list()
  sampled_inputs <- list()
  posterior.files = rep(NA, length(settings$pfts))
  samp <- settings$ensemble$samplingspace
  parents <- lapply(samp, '[[', 'parent')
  order <- names(samp)[lapply(parents, function(tr) which(names(samp) %in% tr)) %>% unlist()]
  samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]

  for (i in seq_along(samp.ordered)) {
    input_tag <- names(samp.ordered)[i]
    parent_name <- samp.ordered[[i]]$parent

    parent_ids <- if (!is.null(parent_name)) sampled_inputs[[parent_name]] else NULL

    input_result <- PEcAn.uncertainty::input.ens.gen(
      settings = settings,
      ensemble_size = ensemble_size,
      input = input_tag,
      method = samp.ordered[[i]]$method,
      parent_ids = parent_ids
    )

    sampled_inputs[[input_tag]] <- input_result$ids
    design_list[[input_tag]] <- input_result$ids
  }

  # Sample parameters
  PEcAn.uncertainty::get.parameter.samples(settings, ensemble.size = ensemble_size, posterior.files, ens.sample.method)

  # Load samples from file
  samples.file <- file.path(settings$outdir, "samples.Rdata")
  samples <- new.env()
  if (file.exists(samples.file)) {
    load(samples.file, envir = samples)
    if (!is.null(samples$ensemble.samples)) {
      # Just a placeholder: extract representative trait index per ensemble member
      # You may want to flatten or select indices per trait
      design_list[["param"]] <- seq_len(ensemble_size)
    } else {
      PEcAn.logger::logger.warn("ensemble.samples not found in samples.Rdata")
    }
  } else {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required")
  }
  design_matrix<- data.frame(design_list)
  
  
  
  
  
  
  
  
  return(design_matrix)
}

