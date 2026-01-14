#!/usr/bin/env Rscript

# This script converts the LandIQ-to-BISm lookup into a packaged dataset.

raw_csv <- file.path("inst", "extdata", "landiq_bsim_lookup.csv")

landiq_bsim_lookup <- utils::read.csv(
    raw_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = c("character", "character", "character")
)

usethis::use_data(landiq_bsim_lookup, overwrite = TRUE)
