#!/usr/bin/env Rscript

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

planting <- list.files(
  config[["planting_events_dir"]],
  "planting_statewide_.*\\.parquet",
  full.names = TRUE
) |>
  arrow::open_dataset() |>
  dplyr::collect() |>
  dplyr::mutate(date = as.Date(.data$date)) |>
  tibble::as_tibble()

planting |>
  dplyr::filter(lubridate::month(date) >= 11) |>
  dplyr::arrange(date) |>
  print(n = 30)

pid <- "101007"  # hay

pid <- "106453" # hay --> row --> hay
pid <- "10726" # woody
pid <- "154989" # woody --> hay --> row --> hay

planting_site <- planting |>
  dplyr::filter(.data$site_id == .env$pid)
harvest <- list.files(
  config[["harvest_events_dir"]],
  ".*\\.parquet",
  full.names = TRUE
) |>
  arrow::open_dataset() |>
  dplyr::filter(.data$site_id == .env$pid) |>
  dplyr::collect() |>
  dplyr::mutate(date = as.Date(.data$date)) |>
  dplyr::as_tibble()
events <- dplyr::bind_rows(planting_site, harvest) |>
  dplyr::arrange(.data$date)
events
