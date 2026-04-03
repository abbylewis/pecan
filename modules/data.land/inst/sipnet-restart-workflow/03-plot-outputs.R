#!/usr/bin/env Rscript

config <- config::get(file = "modules/data.land/inst/sipnet-restart-workflow/config.yml")

outdir <- file.path(config$outdir_root, "out")
ncfiles <- list.files(outdir, full.names = TRUE)
nc <- ncdf4::nc_open(ncfiles[[1]])

results <- PEcAn.utils::read.output(
  ncfiles = ncfiles,
  variables = c("NPP", "GPP", "NEE", "LAI", "AGB", "TotSoilCarb"),
  dataframe = TRUE
)

library(ggplot2)
results |>
  tidyr::pivot_longer(
    -c("posix", "year"),
    names_to = "variable",
    values_to = "value"
  ) |>
  ggplot() +
  aes(x = posix, y = value) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free")
