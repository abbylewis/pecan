context("ssurgo_mukeys")

test_that("ssurgo_mukeys requires exactly one spatial filter", {
  expect_error(ssurgo_mukeys(), "Must provide one of")
  expect_error(ssurgo_mukeys(bbox = c(1, 2, 3, 4), point = c(1, 2)), "Only one of")
  expect_error(ssurgo_mukeys(polygon = matrix(1:10, ncol = 2), point = c(1, 2)), "Only one of")
})

test_that("ssurgo_mukeys validates bbox input", {
  expect_error(ssurgo_mukeys(bbox = "not numeric"), "numeric vector of length 4")
  expect_error(ssurgo_mukeys(bbox = c(1, 2)), "numeric vector of length 4")
  expect_error(ssurgo_mukeys(bbox = c(3, 2, 1, 4)), "xmin < xmax")
  expect_error(ssurgo_mukeys(bbox = c(1, 4, 3, 2)), "ymin < ymax")
})

test_that("ssurgo_mukeys validates point and distance", {
  expect_error(ssurgo_mukeys(point = c(1, 2, 3)), "length 2")
  expect_error(ssurgo_mukeys(point = c(1, 2)), "distance is required")
  expect_error(ssurgo_mukeys(distance = 100), "Must provide one of")
  expect_error(ssurgo_mukeys(point = c(1, 2), distance = -10), "non-negative")
  expect_error(ssurgo_mukeys(point = c(1, 2), distance = "100"), "non-negative")
})

test_that("ssurgo_mukeys validates polygon input", {
  expect_error(ssurgo_mukeys(polygon = 1:5), "sf/sfc object or a matrix")
  expect_error(ssurgo_mukeys(polygon = matrix(1:6, ncol = 3)), "2 columns")
})

test_that("ssurgo_mukeys bbox returns mukeys for valid location", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys(bbox = c(-114.006, 32.1823, -113.806, 32.2823))

  expect_type(mukeys, "character")
  expect_gt(length(mukeys), 0)
})

test_that("ssurgo_mukeys point with distance returns mukeys", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys(point = c(-91.22, 38.46), distance = 600)

  expect_type(mukeys, "character")
  expect_gt(length(mukeys), 0)
})

test_that("ssurgo_mukeys point with zero distance returns mukeys", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys(point = c(-91.22, 38.46), distance = 0)

  expect_type(mukeys, "character")
})

test_that("ssurgo_mukeys polygon as matrix returns mukeys", {
  skip_on_cran()
  skip_on_ci()

  poly <- rbind(
    c(-88.0865046533, 37.5555143852),
    c(-88.0860204771, 37.5600435404),
    c(-88.0782858287, 37.5595392364),
    c(-88.0787704736, 37.5550101113),
    c(-88.0865046533, 37.5555143852)
  )

  mukeys <- tryCatch(
    ssurgo_mukeys(polygon = poly),
    error = function(e) {
      skip(paste("API error:", e$message))
    }
  )

  expect_type(mukeys, "character")
  expect_gt(length(mukeys), 0)
})

test_that("ssurgo_mukeys polygon as sf returns mukeys", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("sf")
  library(sf)

  poly <- rbind(
    c(-88.0865046533, 37.5555143852),
    c(-88.0860204771, 37.5600435404),
    c(-88.0782858287, 37.5595392364),
    c(-88.0787704736, 37.5550101113),
    c(-88.0865046533, 37.5555143852)
  )
  poly_sf <- sf::st_polygon(list(poly))

  mukeys <- tryCatch(
    ssurgo_mukeys(polygon = poly_sf),
    error = function(e) {
      skip(paste("API error:", e$message))
    }
  )

  expect_type(mukeys, "character")
  expect_gt(length(mukeys), 0)
})

test_that("ssurgo_mukeys returns unique mukeys", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys(point = c(-91.22, 38.46), distance = 600)

  expect_equal(length(mukeys), length(unique(mukeys)))
})

test_that("ssurgo_mukeys handles area with no soil data gracefully", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys(bbox = c(0, 0, 0.001, 0.001))

  expect_type(mukeys, "character")
  expect_equal(length(mukeys), 0)
})

test_that("ssurgo_mukeys bbox and point return consistent results for same area", {
  skip_on_cran()
  skip_on_ci()

  center_lon <- -91.22
  center_lat <- 38.46
  distance <- 600

  bbox_mukeys <- ssurgo_mukeys(
    bbox = c(
      center_lon - 0.01,
      center_lat - 0.01,
      center_lon + 0.01,
      center_lat + 0.01
    )
  )
  point_mukeys <- ssurgo_mukeys(
    point = c(center_lon, center_lat),
    distance = distance
  )

  expect_type(bbox_mukeys, "character")
  expect_type(point_mukeys, "character")
  expect_gt(length(bbox_mukeys), length(point_mukeys))
})

test_that("real bounding boxes for CA", {
  # devtools::load_all("modules/data.land")
  bbox_01 <- c(-123.569131, 39.638344, -121.234281, 41.461763)
  bbox_02 <- c(-124.064177, 38.994921, -120.102514, 42.088592)

  expect_error(ssurgo_mukeys(bbox = bbox_01))
  expect_error(ssurgo_mukeys(bbox = bbox_02))
  expect_no_error(ssurgo_mukeys_bigbbox(bbox_01))
  expect_no_error(ssurgo_mukeys_bigbbox(bbox_02))
})
