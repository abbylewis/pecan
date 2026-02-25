#!/usr/bin/env Rscript

# Build the n_application_rate_data packaged dataset from the harmonized
# N application rate CSV produced by the fertilization harmonization
# script in geo. The source CSV lives in the shared geo directory; a copy is
# kept here in data-raw/ for reproducibility.

n_application_rate_data <- readr::read_csv(
  file.path("data-raw", "n_application_rates.csv"),
  col_types = readr::cols(
    pft_group       = readr::col_character(),
    crop            = readr::col_character(),
    min_n_lbs_acre  = readr::col_double(),
    max_n_lbs_acre  = readr::col_double(),
    source          = readr::col_character(),
    min_n_kg_m2     = readr::col_double(),
    max_n_kg_m2     = readr::col_double()
  )
)

usethis::use_data(n_application_rate_data, overwrite = TRUE)
