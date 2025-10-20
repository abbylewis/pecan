# loading libraries.
library(dplyr)
library(xts)
library(PEcAn.all)
library(purrr)
library(furrr)
library(lubridate)
library(nimble)
library(ncdf4)
library(PEcAnAssimSequential)
library(dplyr)
library(sp)
library(raster)
library(zoo)
library(ggplot2)
library(mnormt)
library(sjmisc)
library(stringr)
library(doParallel)
library(doSNOW)
library(Kendall)
library(lgarch)
library(parallel)
library(foreach)
library(terra)
setwd("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/")

# read settings xml file.
settings_dir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/pecan.xml"
settings <- PEcAn.settings::read.settings(settings_dir)

# update settings with the actual PFTs.
settings <- PEcAn.settings::prepare.settings(settings)

# setup the batch job settings.
general.job <- list(cores = 28, folder.num = 35)
batch.settings = structure(list(
  general.job = general.job,
  qsub.cmd = "qsub -l h_rt=24:00:00 -l mem_per_core=4G -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
))
settings$state.data.assimilation$batch.settings <- batch.settings

# alter the ensemble size.
settings$ensemble$size <- 100

# load observations.
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/observation/Rdata/obs_agb_ic_mean.Rdata")
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/observation/Rdata/obs_agb_ic_cov.Rdata")

# replace zero observations and variances with small numbers.
for (i in 1:length(obs.mean)) {
  if(is.null(obs.mean[[i]][[1]])){
    next
  }
  for (j in 1:length(obs.mean[[i]])) {
    if (length(obs.mean[[i]][[j]])==0) {
      next
    }
    obs.mean[[i]][[j]][which(obs.mean[[i]][[j]]==0)] <- 0.01
    if(length(obs.cov[[i]][[j]]) > 1){
      diag(obs.cov[[i]][[j]])[which(diag(obs.cov[[i]][[j]]<=0.1))] <- 0.1
    }else{
      if(obs.cov[[i]][[j]] <= 0.1){
        obs.cov[[i]][[j]] <- 0.1
      }
    }
  }
}

# load PFT parameter file.
load(file.path(settings$outdir, "samples.Rdata"))

# execute the SDA.
PEcAnAssimSequential::qsub_sda(settings = settings, 
                               obs.mean = obs.mean, 
                               obs.cov = obs.cov, 
                               Q = NULL, 
                               pre_enkf_params = NULL, 
                               ensemble.samples = ensemble.samples, 
                               outdir = NULL, 
                               control = list(TimeseriesPlot = FALSE,
                                              OutlierDetection=FALSE,
                                              send_email = NULL,
                                              keepNC = FALSE,
                                              forceRun = TRUE,
                                              MCMC.args = NULL,
                                              merge_nc = TRUE),
                               block.index = NULL,
                               # debias = list(cov.dir = "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/covariates_lc_ts/covariates_nolatlon/", start.year = 2014))
                               cov_dir = "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/covariates_lc_ts/covariates_nolatlon/",
                               debias_start_year = 2013,
                               debias_drop_incomplete_covariates = TRUE,
                               debias_enforce_consistent_obs = FALSE,
                               debias_require_obs_at_t_for_predict = FALSE)

# debug mode.
# folder.path <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/batch/Job_1"
# configs <- readRDS(file.path(folder.path, "configs.rds"))
# settings <- PEcAn.settings::read.settings(configs$setting)
# settings$ensemble$size = 10
# obs.mean <- configs$obs.mean
# obs.cov <- configs$obs.cov
# Q <- configs$Q
# pre_enkf_params <- configs$pre_enkf_params
# ensemble.samples <- configs$ensemble.samples
# outdir <- configs$outdir
# control <- configs$control
# debias <- list(cov.dir = "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/covariates_lc_ts/covariates_nolatlon/", start.year = 2014)
# sda_matchparam = PEcAnAssimSequential:::sda_matchparam
# build_X = PEcAnAssimSequential:::build_X
# analysis_sda_block = PEcAnAssimSequential:::analysis_sda_block
# sda_bias_correction <- PEcAnAssimSequential:::sda_bias_correction
# .get_debias_mod <- PEcAnAssimSequential:::.get_debias_mod


# debug debias module.
# covariates_df = covariates_df_tt                # << use the per-step covariates
# drop_incomplete_covariates = debias_drop_incomplete_covariates
# enforce_consistent_obs     = debias_enforce_consistent_obs
# require_obs_at_t_for_predict = debias_require_obs_at_t_for_predict
# clip_lower_bound = 0.01
# sda.enkf_local(setting, 
#                configs$obs.mean, 
#                configs$obs.cov, 
#                configs$Q, 
#                configs$pre_enkf_params,
#                configs$ensemble.samples,
#                configs$outdir,
#                configs$control,
#                configs$cov_dir, 
#                configs$debias_start_year,
#                configs$debias_drop_incomplete_covariates,
#                configs$debias_enforce_consistent_obs,
#                configs$debias_require_obs_at_t_for_predict)