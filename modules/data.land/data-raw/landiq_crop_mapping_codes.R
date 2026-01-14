#!/usr/bin/env Rscript

# This script converts LandIQ crop mapping codes into a packaged dataset.

raw_tsv <- file.path("inst", "extdata", "landiq_crop_mapping_codes.tsv")

landiq_crop_mapping_codes <- utils::read.delim(
    raw_tsv,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = c("character", "character", "character", "character")
)

usethis::use_data(landiq_crop_mapping_codes, overwrite = TRUE)
