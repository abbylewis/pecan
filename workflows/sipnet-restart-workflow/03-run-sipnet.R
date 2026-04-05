#!/usr/bin/env Rscript

if (FALSE) {
  devtools::install("models/sipnet", upgrade = FALSE)
  devtools::install("modules/data.land", upgrade = FALSE)
}

source("workflows/sipnet-restart-workflow/81-utils.R")

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

outdir_root <- config[["outdir_root"]]

events_json_file <- file.path(outdir_root, "events.json")

settings_raw <- PEcAn.settings::read.settings(file.path(outdir_root, "settings.xml"))
unlink(settings_raw$outdir, recursive = TRUE)
dir.create(settings_raw$outdir, recursive = TRUE, showWarnings = FALSE)

# Get parameter samples for all relevant PFTs
sens_design <- PEcAn.uncertainty::generate_joint_ensemble_design(
  settings_raw,
  settings_raw$ensemble$size
)

settings <- PEcAn.workflow::runModule.run.write.configs(
  settings_raw,
  input_design = sens_design$X
)

inputs_runs <- file.path(settings$outdir, "runs_manifest.csv") |>
  read.csv() |>
  cbind(sens_design[["X"]])

write.csv(inputs_runs, file = file.path(settings$outdir, "inputs_runs.csv"))

# Begin loop
for (irun in seq_len(nrow(inputs_runs))) {
  run_row <- inputs_runs[irun, ]
  run_sipnet_segmented(settings, run_row, events_json_file)
}
