#' build_X
#' 
#' @name build_X
#' @author Alexis Helgeson
#' 
#' @description builds X matrix for SDA
#'
#' @param new.params object created from sda_matchparam, passed from sda.enkf_MultiSite
#' @param nens number of ensemble members i.e. runs
#' @param read_restart_times passed from sda.enkf_MultiSite
#' @param settings settings object, passed from sda.enkf_MultiSite
#' @param outdir location of previous run output folder containing .nc files
#' @param out.configs object created for build_X passed from sda.enkf_MultiSite
#' @param t Default t=1, for function to work within time loop
#' @param var.names list of state variables taken from settings object
#' @param my.read_restart object that points to the model restart function i.e. read_restart.SIPNET
#' @param restart_flag flag if it's a restart stage. Default is FALSE.
#'
#' @return X ready to be passed to SDA Analysis code
build_X <- function(out.configs, settings, new.params, nens, read_restart_times, outdir, t = 1, var.names, my.read_restart, restart_flag = FALSE){

  # Single site: Parallel by ensemble
  if (length(settings) == 1) {
    
    my_settings <- settings[[1]]
    cfg         <- out.configs[[1]]
    siteparams  <- new.params[[1]]
    
    ens_ids <- seq_len(nens)
    
    if (t == 1 && restart_flag) {
      reads_site <- furrr::future_map(ens_ids, function(i) {
        library(paste0("PEcAn.", my_settings$model$type), character.only = TRUE)
        runid_i <- as.character(my_settings$run$id[i])
        
        do.call(
          my.read_restart,
          args = list(
            outdir    = outdir,
            runid     = runid_i,
            stop.time = read_restart_times[t + 1],
            settings  = my_settings,
            var.names = var.names,
            params    = siteparams[[i]]
          )
        )
      })
      
    } else {
      reads_site <- furrr::future_map(ens_ids, function(i) {
        
        library(paste0("PEcAn.", my_settings$model$type), character.only = TRUE)
        runid_i <- as.character(cfg$runs$id[i])
        
        do.call(
          my.read_restart,
          args = list(
            outdir    = outdir,
            runid     = runid_i,
            stop.time = read_restart_times[t + 1],
            settings  = my_settings,
            var.names = var.names,
            params    = siteparams[[i]]
          )
        )
      })
    }
    
    reads <- list(reads_site)
    
    # Multi-site: Retain the original parallel writing method by site
  } else {
    
    reads <-
      furrr::future_pmap(
        list(out.configs %>% `class<-`(c("list")),
             settings,
             new.params),
        function(configs, my_settings, siteparams) {
          
          X_tmp <- vector("list", nens)
          
          if (t == 1 && restart_flag) {
            for (i in seq_len(nens)) {
              X_tmp[[i]] <- do.call(
                my.read_restart,
                args = list(
                  outdir    = outdir,
                  runid     = as.character(my_settings$run$id[i]),
                  stop.time = read_restart_times[t + 1],
                  settings  = my_settings,
                  var.names = var.names,
                  params    = siteparams[[i]]
                )
              )
            }
          } else {
            for (i in seq_len(nens)) {
              X_tmp[[i]] <- do.call(
                my.read_restart,
                args = list(
                  outdir    = outdir,
                  runid     = as.character(configs$runs$id[i]),
                  stop.time = read_restart_times[t + 1],
                  settings  = my_settings,
                  var.names = var.names,
                  params    = siteparams[[i]]
                )
              )
            }
          }
          
          return(X_tmp)
        })
  }
  
  return(reads)
}