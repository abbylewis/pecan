context("write.events.SIPNET")

# Helper to remove excess whitespace
norm <- function(x) gsub("\\s+", " ", trimws(x))

# Helper to locate fixtures from PEcAn.data.land, with source fallback
fixture_path <- function(name) {
    p <- system.file(file.path("events_fixtures", name), package = "PEcAn.data.land")
    if (!is.null(p) && nzchar(p)) return(p)
    # Fallback to source path in monorepo when PEcAn.data.land isn't installed
    testthat::test_path(file.path("../../../../modules/data.land/inst/events_fixtures", name))
}

testthat::test_that("write.events.SIPNET produces expected lines", {
    ev_json1 <- fixture_path("events_site1.json")
    outdir <- withr::local_tempdir()
    files <- write.events.SIPNET(ev_json1, outdir)
    expect_length(files, 1)
    got <- readLines(files[1])
    expected <- c(
        "2022  35  till  0.2",
        "2022  40  till  0.1",
        "2022  40  irrig  5 1",
        "2022  40  fert   0 0 10",
        "2022  50  plant  10 3 2 5",
        "2022  250 harv   0.1 0 0 0"
    )
    expect_equal(norm(got), norm(expected))
    # TODO determine What's generating the whitespace differences and eliminate use of norm()
})

testthat::test_that("write.events.SIPNET handles multi-site events.json (one file per site)", {
    ev_json2 <- fixture_path("events_site1_site2.json")
    outdir <- withr::local_tempdir()
    files <- write.events.SIPNET(ev_json2, outdir)
    testthat::expect_length(files, 2)
    testthat::expect_true(all(file.exists(files)))
    # quick sanity checks for each site's first/last event ordering
    got1 <- readLines(files[grepl("events-S1\\.in$", files)])
    got2 <- readLines(files[grepl("events-S2\\.in$", files)])
    testthat::expect_true(startsWith(norm(got1[1]), "2022 15 till"))
    testthat::expect_true(startsWith(norm(tail(got1, 1)), "2022 244 harv"))
    testthat::expect_true(startsWith(norm(got2[1]), "2022 60 plant"))
    testthat::expect_true(startsWith(norm(tail(got2, 1)), "2022 69 irrig"))
})
