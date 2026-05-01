context("Spatial Covariance Calculation")

test_that("calcSpatialCov.matrix returns correct format, dimensions, and values", {
  d <- matrix(c(0, 1, 1, 0), 2, 2)
  psi <- 0.5
  tau <- 2.0
  res <- calcSpatialCov.matrix(d, psi, tau)
  
  expect_true(is.matrix(res))
  expect_equal(dim(res), c(2, 2))
  
  expected <- tau * exp(-psi * d)
  expect_equal(res, expected)
})
