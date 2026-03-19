
count_file_lines <- function(path) {
  system2("wc", c("-l", path), stdout = TRUE) |>
    trimws() |>
    strsplit("\\s+") |>
    sapply("[[", 1) |>
    as.integer()
}

test_that("split_inputs", {

  climfile <- system.file("niwot.clim", package = "PEcAn.SIPNET")
  outdir <- withr::local_tempdir()

  dates <- seq(
    from = as.Date("1998-11-01"),
    to = as.Date("2005-12-31"),
    by = "2 years"
  )

  clim_split <- mapply(
    split_inputs.SIPNET,
    start.time = dates,
    stop.time = c(dates[-1], as.Date("2006-01-01")), # Stop just _before_ these dates
    MoreArgs = list(
      inputs = climfile,
      outpath = outdir
    )
  )

  # all steps processed
  expect_length(clim_split, 4)
  expect_true(all(file.exists(clim_split)))

  # All lines appear in exactly 1 split file
  expect_equal(
    clim_split |> sapply(count_file_lines) |> sum(),
    count_file_lines(climfile)
  )
  # Lines are numerically identical
  # NB raw text does differ (split_inputs changes some `0.000` to `0`, etc),
  # but should parse equal when read as numeric.
  expect_equal(
    read.table(climfile, nrows = 5),
    read.table(clim_split[1], nrows = 5)
  )
  expect_equal(
    read.table(text = system2("tail", c("-n5", climfile), stdout = TRUE)),
    read.table(text = system2("tail", c("-n5", clim_split[4]), stdout = TRUE))
  )

})

test_that("v2 clim format", {
  outdir <- withr::local_tempdir()
  climfile <- file.path(outdir, "niwot_v2.clim")
  v1_clim <- system.file("niwot.clim", package = "PEcAn.SIPNET")
  system2("cut", c("-d' '", "-f2-13", v1_clim, ">", climfile))

  clim_split <- split_inputs.SIPNET(
    start.time = "2001-01-01",
    stop.time = "2001-04-01",
    inputs = climfile,
    outpath = outdir
  )
  expect_length(clim_split, 1)
  expect_equal(count_file_lines(clim_split), 90 * 2) # Jan-March 2x/day
  expect_equal(ncol(read.table(clim_split, nrows = 1)), 12)
})
