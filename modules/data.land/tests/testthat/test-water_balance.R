context("Water balance calculations")

expect_nonnegative <- function(result) {
  testthat::expect_true(all(result$W_t >= 0))
  testthat::expect_true(all(result$irr >= 0))
  testthat::expect_true(all(result$runoff >= 0))
}

test_that("calc_water_balance: more precip leads to more runoff", {
  n <- 10
  et <- rep(5, n)
  whc <- 100
  whc_min_frac <- 0.5

  precip_low <- c(rep(5, 5), rep(0, 5))
  precip_high <- c(rep(15, 5), rep(0, 5))

  result_low <- calc_water_balance(et, precip_low, whc, whc_min_frac)
  result_high <- calc_water_balance(et, precip_high, whc, whc_min_frac)

  expect_true(sum(result_high$runoff) > sum(result_low$runoff))
  expect_nonnegative(result_low)
  expect_nonnegative(result_high)
})

test_that("calc_water_balance: more ET leads to less runoff", {
  n <- 10
  precip <- c(rep(10, 5), rep(0, 5))
  whc <- 100
  whc_min_frac <- 0.5

  et_low <- rep(2, n)
  et_high <- rep(8, n)

  result_low <- calc_water_balance(et_low, precip, whc, whc_min_frac)
  result_high <- calc_water_balance(et_high, precip, whc, whc_min_frac)

  expect_true(sum(result_high$runoff) < sum(result_low$runoff))
  expect_nonnegative(result_low)
  expect_nonnegative(result_high)
})

test_that("calc_water_balance: more ET leads to more irrigation", {
  n <- 60
  precip <- rep(0, n)
  whc <- 100
  whc_min_frac <- 0.5

  et_low <- rep(1, n)
  et_high <- rep(5, n)

  result_low <- calc_water_balance(et_low, precip, whc, whc_min_frac)
  result_high <- calc_water_balance(et_high, precip, whc, whc_min_frac)

  expect_true(sum(result_high$irr) > sum(result_low$irr))
  expect_nonnegative(result_low)
  expect_nonnegative(result_high)
})
