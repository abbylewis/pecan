#!/usr/bin/env Rscript

if (FALSE) {
  devtools::install("models/sipnet", upgrade = FALSE)
  devtools::install("modules/data.land", upgrade = FALSE)
}

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

settings <- PEcAn.settings::read.settings(file.path(config[["outdir_root"]], "settings.xml"))
inputs_runs <- read.csv(file.path(settings$outdir, "inputs_runs.csv"))

source("workflows/sipnet-restart-workflow/utils.R")

for (i in seq_len(nrow(inputs_runs))) {
  run_sipnet_segmented(settings, inputs_runs[i, ], replace_and_link = TRUE)
}

PEcAn.workflow::runModule_start_model_runs(settings)
