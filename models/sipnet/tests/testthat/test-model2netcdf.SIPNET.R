test_that("model2netcdf.SIPNET produces netCDF from v2 output with GHG fluxes", {
  outdir <- withr::local_tempdir(pattern = "sipnet_out_")
  rundir <- withr::local_tempdir(pattern = "sipnet_run_")

  # minimal sipnet.param — model2netcdf only reads leafCSpWt for LAI
  writeLines(
    c("plantWoodInit\t30000\t0\t6600\t14000\t200",
      "leafCSpWt\t32\t0\t13\t500\t0"),
    file.path(rundir, "sipnet.param")
  )

  # synthesise a 4-timestep sipnet.out (2 days x 12-hourly)
  n <- 4L
  ts_s <- 43200  # 12h in sec
  sipnet_dat <- data.frame(
    year = 2002, day = c(1, 1, 2, 2), time = c(6, 18, 6, 18),
    plantWoodC = 5000, plantLeafC = 200, woodCreation = 0.5,
    soil = 10000, microbeC = 8, coarseRootC = 1200, fineRootC = 800,
    litter = 400, soilWater = 14, soilWetnessFrac = 0.85, snow = 0,
    npp = 0.05, nee = 0.10, cumNEE = cumsum(rep(0.1, n)),
    gpp = 0.30, rAboveground = 0.04, rSoil = 0.09, rRoot = 0.01,
    ra = 0.05, rh = 0.08, rtot = 0.13,
    evapotranspiration = 0.005, fluxestranspiration = 0.003,
    n2oFlux = 0.002,
    ch4Flux = 0.001
  )

  out_path <- file.path(outdir, "sipnet.out")
  writeLines(
    "Notes: units in g/m2 per timestep; water in cm",
    out_path
  )
  suppressWarnings(
    write.table(sipnet_dat, file = out_path, append = TRUE,
                row.names = FALSE, quote = FALSE, sep = "\t")
 )

  # outdir must contain "/out/" so the gsub to find "/run/" works
  # we've set up rundir separately, so patch the path convention:
  # model2netcdf does gsub("/out/", "/run/", outdir) to find sipnet.param
  # use the structure: <base>/out/run1  and  <base>/run/run1
  base <- withr::local_tempdir(pattern = "sipnet_base_")
  real_outdir <- file.path(base, "out", "run1")
  real_rundir <- file.path(base, "run", "run1")
  dir.create(real_outdir, recursive = TRUE)
  dir.create(real_rundir, recursive = TRUE)
  file.copy(out_path, file.path(real_outdir, "sipnet.out"))
  writeLines(
    c("plantWoodInit\t30000\t0\t6600\t14000\t200",
      "leafCSpWt\t32\t0\t13\t500\t0"),
    file.path(real_rundir, "sipnet.param")
  )

  suppressMessages(
    model2netcdf.SIPNET(
      outdir     = real_outdir,
      sitelat    = 38.0,
      sitelon    = -121.0,
      start_date = "2002-01-01",
      end_date   = "2002-12-31",
      delete.raw = FALSE,
      revision   = "r136"
    )
  )

  nc_file <- file.path(real_outdir, "2002.nc")
  expect_true(file.exists(nc_file))

  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)

  # -- GHG variables --
  expect_true("N2O_flux" %in% vars)
  expect_true("CH4_flux" %in% vars)

  # -- standard C-cycle variables --
  expect_true(all(c("GPP", "NEE", "TotalResp", "TotSoilCarb") %in% vars))

  # -- g m-2 per timestep -> kg m-2 s-1 --
  n2o <- as.numeric(ncdf4::ncvar_get(nc, "N2O_flux"))
  ch4 <- as.numeric(ncdf4::ncvar_get(nc, "CH4_flux"))
  gpp <- as.numeric(ncdf4::ncvar_get(nc, "GPP"))

  expect_equal(n2o, rep(0.002 * 1e-3 / ts_s, n), tolerance = 1e-12)
  expect_equal(ch4, rep(0.001 * 1e-3 / ts_s, n), tolerance = 1e-12)
  expect_equal(gpp, rep(0.30  * 1e-3 / ts_s, n), tolerance = 1e-12)

  expect_equal(nc$var$N2O_flux$units, "kg N m-2 s-1")
  expect_equal(nc$var$CH4_flux$units, "kg C m-2 s-1")
  expect_equal(nc$var$GPP$units,      "kg C m-2 s-1")

  # -- dimensions --
  expect_equal(nc$dim$time$len, n)
  expect_true(grepl("days since 2002", nc$dim$time$units))
})


test_that("model2netcdf.SIPNET omits N2O/CH4 when columns absent (backward compat)", {
  base <- withr::local_tempdir(pattern = "sipnet_v1_")
  real_outdir <- file.path(base, "out", "run1")
  real_rundir <- file.path(base, "run", "run1")
  dir.create(real_outdir, recursive = TRUE)
  dir.create(real_rundir, recursive = TRUE)

  writeLines(
    c("plantWoodInit\t30000\t0\t6600\t14000\t200",
      "leafCSpWt\t32\t0\t13\t500\t0"),
    file.path(real_rundir, "sipnet.param")
  )

  sipnet_dat <- data.frame(
    year = 2002, day = c(1, 1), time = c(6, 18),
    plantWoodC = 5000, plantLeafC = 200, woodCreation = 0.5,
    soil = 10000, microbeC = 8, coarseRootC = 1200, fineRootC = 800,
    litter = 400, soilWater = 14, soilWetnessFrac = 0.85, snow = 0,
    npp = 0.05, nee = 0.10, cumNEE = c(0.1, 0.2),
    gpp = 0.30, rAboveground = 0.04, rSoil = 0.09, rRoot = 0.01,
    ra = 0.05, rh = 0.08, rtot = 0.13,
    evapotranspiration = 0.005, fluxestranspiration = 0.003
  )

  out_path <- file.path(real_outdir, "sipnet.out")
  writeLines("Notes: g/m2", out_path)
  suppressWarnings(
    write.table(sipnet_dat, file = out_path, append = TRUE,
                row.names = FALSE, quote = FALSE, sep = "\t")
  )

  suppressMessages(
    model2netcdf.SIPNET(
      outdir     = real_outdir,
      sitelat    = 38.0,
      sitelon    = -121.0,
      start_date = "2002-01-01",
      end_date   = "2002-12-31",
      delete.raw = FALSE,
      revision   = "r136"
    )
  )

  nc <- ncdf4::nc_open(file.path(real_outdir, "2002.nc"))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)

  expect_false("N2O_flux" %in% vars)
  expect_false("CH4_flux" %in% vars)
  expect_true("GPP" %in% vars)
})


test_that("delete.raw removes sipnet.out after conversion", {
  base <- withr::local_tempdir(pattern = "sipnet_del_")
  real_outdir <- file.path(base, "out", "run1")
  real_rundir <- file.path(base, "run", "run1")
  dir.create(real_outdir, recursive = TRUE)
  dir.create(real_rundir, recursive = TRUE)

  writeLines(
    c("plantWoodInit\t30000\t0\t6600\t14000\t200",
      "leafCSpWt\t32\t0\t13\t500\t0"),
    file.path(real_rundir, "sipnet.param")
  )

  sipnet_dat <- data.frame(
    year = 2002, day = 1, time = 12,
    plantWoodC = 5000, plantLeafC = 200, woodCreation = 0.5,
    soil = 10000, microbeC = 8, coarseRootC = 1200, fineRootC = 800,
    litter = 400, soilWater = 14, soilWetnessFrac = 0.85, snow = 0,
    npp = 0.05, nee = 0.10, cumNEE = 0.1,
    gpp = 0.30, rAboveground = 0.04, rSoil = 0.09, rRoot = 0.01,
    ra = 0.05, rh = 0.08, rtot = 0.13,
    evapotranspiration = 0.005, fluxestranspiration = 0.003
  )

  raw_path <- file.path(real_outdir, "sipnet.out")
  writeLines("Notes: g/m2", raw_path)
  suppressWarnings(
    write.table(sipnet_dat, file = raw_path, append = TRUE,
                row.names = FALSE, quote = FALSE, sep = "\t")
  )

  suppressMessages(
    model2netcdf.SIPNET(
      outdir     = real_outdir,
      sitelat    = 38.0,
      sitelon    = -121.0,
      start_date = "2002-01-01",
      end_date   = "2002-12-31",
      delete.raw = TRUE,
      revision   = "r136"
    )
  )

  expect_false(file.exists(raw_path))
  expect_true(file.exists(file.path(real_outdir, "2002.nc")))
})