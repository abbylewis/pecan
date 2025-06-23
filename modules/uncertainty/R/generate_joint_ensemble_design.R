generate_joint_ensemble_design <- function(settings, ensemble_size, posterior.files, ens.sample.method) {
  design_matrix <- data.frame()
  sampled_inputs <- list()

  # Get and order inputs based on dependencies
  samp <- settings$ensemble$samplingspace
  parents <- lapply(samp, '[[', 'parent')
  order <- names(samp)[lapply(parents, function(tr) which(names(samp) %in% tr)) %>% unlist()]
  samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]

  # Generate input sampling design
  for (i in seq_along(samp.ordered)) {
    input_tag <- names(samp.ordered)[i]
    parent_name <- samp.ordered[[i]]$parent

    parent_ids <- if (!is.null(parent_name)) sampled_inputs[[parent_name]] else NULL

    input_result <- PEcAn.uncertainty::input.ens.gen(
      settings = settings,
      input = input_tag,
      method = samp.ordered[[i]]$method,
      parent_ids = parent_ids
    )

    sampled_inputs[[input_tag]] <- input_result$ids
    design_matrix[[input_tag]] <- input_result$ids
  }

  # Load parameter sample indices
  PEcAn.uncertainty::get.parameter.samples(settings, posterior.files, ens.sample.method)
  samples.file <- file.path(settings$outdir, "samples.Rdata")
  if (!file.exists(samples.file)) {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required.")
  }

  samples <- new.env()
  load(samples.file, envir = samples)
  ensemble.samples <- samples$ensemble.samples

  pft_name <- names(ensemble.samples)[1]
  first_trait <- names(ensemble.samples[[pft_name]])[1]
  param_ids <- seq_len(nrow(ensemble.samples[[pft_name]][[first_trait]]))

  
  design_matrix$parameters <- param_ids[1:ensemble_size]

  return(design_matrix)
}
