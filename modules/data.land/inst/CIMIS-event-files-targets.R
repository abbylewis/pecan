#' ---
#' title: "Example workflow generating SIPNET event files from CIMIS and CHIRPS data"
#' author: "Alexey N. Shiklomanov"
#' ---

library(targets)

targets_file <- here::here("_targets.R")
targets_store <- here::here("_targets/")
tar_config_set(
  script = targets_file,
  store = targets_store
)

#' Write the targets pipeline script to _targets.R in this directory.
tar_script(
  code = {
    library(targets)
    library(tarchetypes)

    # if (interactive()) {
    devtools::load_all(here::here("modules/data.land"))
    # } else {
    #   library(PEcAn.data.land)
    # }

    # -------------------------------------------------------------------------
    # Helper functions
    # -------------------------------------------------------------------------

    #' Generate a sequence of dates for a given year/season (1=winter, 2=spring,
    #' 3=summer, 4=fall).
    fill_season <- function(year, season) {
      if (season == 1) {
        start <- lubridate::make_date(year, 1, 1)
        end   <- lubridate::make_date(year, 3, 31)
      } else if (season == 2) {
        start <- lubridate::make_date(year, 4, 1)
        end   <- lubridate::make_date(year, 6, 30)
      } else if (season == 3) {
        start <- lubridate::make_date(year, 7, 1)
        end   <- lubridate::make_date(year, 9, 30)
      } else if (season == 4) {
        start <- lubridate::make_date(year, 10, 1)
        end   <- lubridate::make_date(year, 12, 31)
      }
      seq.Date(start, end, "day")
    }

    #' Calculate effective available water capacity (mm) for a soil profile
    #' clipped to a given rooting depth.
    calc_effective_awc <- function(
      hzdept_r_cm,
      hzdepb_r_cm,
      awc_r,
      rooting_depth_cm
    ) {
      effective_top    <- pmin(hzdept_r_cm, rooting_depth_cm)
      effective_bottom <- pmin(hzdepb_r_cm, rooting_depth_cm)
      thickness_cm     <- pmax(0, effective_bottom - effective_top)
      # awc_r is cm water / cm soil; multiply by thickness -> cm water -> mm water
      sum(awc_r * thickness_cm, na.rm = TRUE) * 10
    }

    #' Average ETc and WHC across multi-crop parcels (double-cropping hack).
    resolve_multicrop <- function(etc_data, id_col = "id", date_col = "date") {
      id_sym   <- rlang::sym(id_col)
      date_sym <- rlang::sym(date_col)

      multicrop_counts <- etc_data |>
        dplyr::add_count(!!id_sym, !!date_sym, name = "n") |>
        dplyr::filter(.data$n > 1) |>
        dplyr::summarize(
          n_multicrop = dplyr::n_distinct(!!id_sym, !!date_sym),
          .groups = "drop"
        )

      if (multicrop_counts$n_multicrop > 0) {
        message(
          "Multi-crop parcels: ",
          multicrop_counts$n_multicrop,
          " date-parcel combinations have multiple crops. Averaging ETc and WHC values."
        )
      }

      etc_data |>
        dplyr::group_by(!!id_sym, !!date_sym) |>
        dplyr::summarize(
          etc_mm_day   = mean(.data$etc_mm_day,   na.rm = TRUE),
          whc_min_frac = mean(.data$whc_min_frac, na.rm = TRUE),
          whc_mm       = mean(.data$whc_mm,       na.rm = TRUE),
          .groups = "drop"
        )
    }

    # -------------------------------------------------------------------------
    # Package options
    # -------------------------------------------------------------------------

    tar_option_set(
      packages = c(
        "dplyr", "tidyr", "purrr", "readr", "tibble",
        "lubridate", "glue", "ggplot2", "arrow", "rlang"
      )
    )

    # -------------------------------------------------------------------------
    # Pipeline
    # -------------------------------------------------------------------------

    list(

      # --- Inputs from environment variables ---------------------------------

      tar_target(design_points_path, Sys.getenv("DESIGN_POINTS")),
      tar_target(cimis_eto_cog_path, Sys.getenv("CIMIS_ETO_COG")),
      tar_target(parcels_path,       Sys.getenv("LANDIQ_PARCELS")),
      tar_target(crops_path,         Sys.getenv("LANDIQ_CROPS")),
      tar_target(event_output_dir,   path.expand(Sys.getenv("EVENT_OUTPUT_DIR"))),

      # --- Validate all input paths exist ------------------------------------

      tar_target(validated_paths, {
        stopifnot(
          file.exists(design_points_path),
          dir.exists(cimis_eto_cog_path),
          file.exists(parcels_path),
          file.exists(crops_path)
        )
        dir.create(event_output_dir, showWarnings = FALSE, recursive = TRUE)
        TRUE
      }),

      # --- Base inputs -------------------------------------------------------

      tar_target(
        dates,
        seq.Date(as.Date("2020-03-01"), as.Date("2020-11-30"), "day")
      ),

      tar_target(
        design_points,
        readr::read_csv(design_points_path, show_col_types = FALSE) |> head(10)
      ),

      # --- Remote data extractions (slow; most benefit from caching) ---------

      tar_target(
        etref,
        design_points |>
          extract_cimis_dates(dates, cimis_eto_cog_path, .progress = TRUE)
      ),

      tar_target(
        precip,
        extract_chirps_remote(design_points, dates)
      ),

      # --- LandIQ crop data --------------------------------------------------

      tar_target(
        dp_with_crops,
        get_landiq(
          design_points,
          parcels_file = parcels_path,
          crops_file   = crops_path
        ) |>
          tibble::as_tibble()
      ),

      #' NOTE: Some LandIQ classes/subclasses map onto multiple BISM crop types.
      #' HACK: select just the first crop per class/subclass group.
      tar_target(
        bism_crop_unique,
        bism_kc_by_crop |>
          dplyr::distinct(landiq_class, landiq_subclass, crop_name) |>
          dplyr::slice(1, .by = c("landiq_class", "landiq_subclass"))
      ),

      tar_target(
        design_point_crops,
        dp_with_crops |>
          dplyr::left_join(
            bism_crop_unique,
            by = c("CLASS" = "landiq_class", "SUBCLASS" = "landiq_subclass")
          )
      ),

      #' Expand crop seasons to daily rows using hard-coded quarterly dates.
      #' In reality these would be resolved from phenology data.
      tar_target(
        dp_crops_filled,
        design_point_crops |>
          dplyr::filter(!is.na(season)) |>
          tidyr::fill(
            "CLASS", "SUBCLASS", "crop_name",
            .direction = "downup",
            .by = "parcel_id"
          ) |>
          dplyr::mutate(date = purrr::map2(year, season, fill_season)) |>
          tidyr::unnest(date) |>
          dplyr::filter(date %in% dates)
      ),

      #' Warn about parcels with no matching BIS crop, then filter them out.
      tar_target(
        dp_with_cropname, {
          missing_crops <- dp_crops_filled |> dplyr::filter(is.na(crop_name))
          if (nrow(missing_crops) > 0) {
            missing_crop_strs <- missing_crops |>
              dplyr::distinct(CLASS, SUBCLASS) |>
              dplyr::mutate(
                string = glue::glue("CLASS: {CLASS} SUBCLASS: {SUBCLASS}")
              ) |>
              dplyr::pull(string)
            warning(
              "Skipping ", nrow(missing_crops),
              " rows with no matching BIS crop. Relevant pairs are: [",
              paste(missing_crop_strs, collapse = "; "), "]"
            )
          }
          dp_crops_filled |>
            dplyr::filter(!is.na(crop_name)) |>
            dplyr::left_join(
              crop_whc |>
                dplyr::select("crop_name", "whc_min_frac", "rooting_depth_m"),
              by = "crop_name"
            )
        }
      ),

      # --- SSURGO soil data --------------------------------------------------

      tar_target(
        design_points_sf,
        dplyr::distinct(design_points, id, lon, lat)
      ),

      tar_target(
        mukeys_list,
        purrr::map2(
          design_points_sf$lon,
          design_points_sf$lat,
          ~ ssurgo_mukeys_point(point = c(.x, .y), distance = 20)
        )
      ),

      tar_target(
        soil_raw,
        gSSURGO.Query(
          mukeys = unique(unlist(mukeys_list)),
          fields = c("chorizon.awc_r", "chorizon.hzdept_r", "chorizon.hzdepb_r")
        )
      ),

      tar_target(
        soil_dominant,
        soil_raw |>
          dplyr::filter(cokey == cokey[which.max(comppct_r)], .by = "mukey")
      ),

      tar_target(
        dp_with_whc,
        dp_with_cropname |>
          dplyr::mutate(
            mukey = mukeys_list[match(id, design_points_sf$id)]
          ) |>
          tidyr::unnest(mukey) |>
          dplyr::mutate(mukey = as.numeric(mukey)) |>
          dplyr::left_join(
            soil_dominant,
            by = "mukey",
            relationship = "many-to-many"
          ) |>
          dplyr::summarize(
            whc_mm = calc_effective_awc(
              hzdept_r, hzdepb_r, awc_r,
              rooting_depth_cm = rooting_depth_m[[1]] * 100
            ),
            .by = c("id", "parcel_id", "date", "crop_name", "whc_min_frac")
          ) |>
          dplyr::mutate(
            whc_mm = dplyr::if_else(whc_mm > 0, whc_mm, 500, missing = 500)
          )
      ),

      # --- ETc and water balance ---------------------------------------------

      tar_target(
        dp_with_eto,
        dp_with_whc |>
          dplyr::left_join(
            etref |> dplyr::select("id", "date", "etref_mm_day"),
            by = c("id", "date")
          )
      ),

      tar_target(
        dp_with_etc,
        dp_with_eto |>
          dplyr::mutate(
            etc_mm_day = eto_to_etc_bism(
              eto       = etref_mm_day,
              crop_name = crop_name[[1]],
              date      = date
            ),
            .by = "crop_name"
          ) |>
          dplyr::select(
            dplyr::any_of(c("id", "parcel_id", "lat", "lon")),
            "date", "etc_mm_day", "whc_min_frac", "whc_mm"
          ) |>
          resolve_multicrop()
      ),

      tar_target(
        dp_crops_all,
        dp_with_etc |>
          dplyr::inner_join(precip, by = c("id", "date")) |>
          dplyr::select(
            "id", "lat", "lon", "date",
            "etc_mm_day", "precip_mm_day", "whc_min_frac", "whc_mm"
          )
      ),

      tar_target(
        dpwb,
        apply_water_balance(dp_crops_all, "id")
      ),

      # --- Diagnostics -------------------------------------------------------

      tar_target(
        etc_summary,
        dp_crops_all |>
          dplyr::summarize(
            etc_min  = min(.data$etc_mm_day,  na.rm = TRUE),
            etc_max  = max(.data$etc_mm_day,  na.rm = TRUE),
            etc_mean = mean(.data$etc_mm_day, na.rm = TRUE),
            .by = "id"
          )
      ),

      tar_target(
        wb_summary,
        dpwb |>
          dplyr::group_by(.data$id) |>
          dplyr::summarize(
            irr_total    = sum(.data$irr,   na.rm = TRUE),
            irr_max      = max(.data$irr,   na.rm = TRUE),
            irr_mean     = mean(.data$irr,  na.rm = TRUE),
            runoff_total = sum(.data$runoff, na.rm = TRUE),
            W_t_min      = min(.data$W_t,   na.rm = TRUE),
            W_t_max      = max(.data$W_t,   na.rm = TRUE),
            .groups = "drop"
          ) |>
          (\(x) {
            print(x)
            if (any(x$irr_max < 0))  warning("Negative irrigation values detected!")
            else                      message("Irrigation values are non-negative")
            if (any(x$W_t_min < 0))  warning("Negative soil water values detected!")
            else                      message("Soil water values are non-negative")
            x
          })()
      ),

      tar_target(
        monthly_irr,
        dpwb |>
          dplyr::mutate(month = lubridate::month(.data$date)) |>
          dplyr::group_by(.data$month) |>
          dplyr::summarize(irr_mean = mean(.data$irr, na.rm = TRUE), .groups = "drop") |>
          (\(x) { print(x); x })()
      ),

      # --- Plot (saved as PNG) -----------------------------------------------

      tar_target(
        irrigation_plot, {
          p <- dpwb |>
            ggplot2::ggplot() +
            ggplot2::aes(x = date, y = irr, color = id) +
            ggplot2::geom_line() +
            ggplot2::labs(
              title = "Irrigation Requirements by Site",
              y     = "Irrigation (mm/day)"
            )
          path <- file.path(event_output_dir, "irrigation_plot.png")
          ggplot2::ggsave(path, p, width = 10, height = 6)
          path
        },
        format = "file"
      ),

      # --- Write SIPNET event files ------------------------------------------

      tar_target(
        event_files, {
          dpwb |>
            dplyr::group_nest(.data$id) |>
            dplyr::mutate(
              fname = purrr::map2(
                id, data,
                \(site_id, dat) {
                  readr::write_delim(
                    create_event_file(dat),
                    file.path(
                      event_output_dir,
                      glue::glue("{site_id}_events.txt")
                    ),
                    delim      = " ",
                    col_names  = FALSE
                  )
                }
              )
            )
          list.files(event_output_dir, full.names = TRUE,
                     pattern = "_events\\.txt$")
        },
        format = "file"
      )

    )
  },
  ask = FALSE
)

#' Run the pipeline. Targets that are already up-to-date will be skipped.
tar_make()
