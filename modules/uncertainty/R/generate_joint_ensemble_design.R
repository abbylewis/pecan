generate_joint_ensemble_design<- function(settings, ensemble_size) {
  design_matrix <- data.frame()
  sampled_inputs <- list()
  
  samp <- settings$ensemble$samplingspace
  parents <- lapply(samp, '[[', 'parent')
  
  order <- names(samp)[lapply(parents, function(tr) which(names(samp) %in% tr)) %>% unlist()]
  samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]

  for (i in seq_along(samp.ordered)) {
    input_tag <- names(samp.ordered)[i]
    parent_name <- samp.ordered[[i]]$parent
    
    if (!is.null(parent_name)) {
      parent_ids <- sampled_inputs[[parent_name]]
    } else {
      parent_ids <- NULL
    }

    input_result <- PEcAn.uncertainty::input.ens.gen(
      settings = settings,
      input = input_tag,
      method = samp.ordered[[i]]$method,
      parent_ids = parent_ids
    )

    sampled_inputs[[input_tag]] <- input_result
    design_matrix[[input_tag]] <- input_result$ids
  }

  return(design_matrix)
}
