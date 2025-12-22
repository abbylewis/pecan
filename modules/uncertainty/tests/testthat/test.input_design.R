# Tests for input design generation functions:
# - generate_OAT_SA_design (sensitivity analysis)
# - generate_joint_ensemble_design (ensemble analysis)

# helper for minimal settings for SA design testing
make_sa_test_settings <- function() {
  list(
    outdir = withr::local_tempdir(),
    pfts = list(list(name = "pft1")),
    ensemble = list(
      samplingspace = list(
        parameters = list(method = "uniform"),
        met = list(method = "sampling")
      )
    )
  )
}

# helper to mock sa.samples structure
make_mock_sa_samples <- function() {
  list(
    pft1 = structure(
      matrix(1:9, nrow = 3, ncol = 3),
      dimnames = list(c("25", "50", "75"), c("trait1", "trait2", "trait3"))
    )
  )
}

test_that("generate_OAT_SA_design returns correct structure and run count", {
  settings <- make_sa_test_settings()
  sa_samples <- make_mock_sa_samples()
  
  result <- generate_OAT_SA_design(settings, sa_samples = sa_samples)
  
  # 1 median + 3 traits * 2 non-median quantiles = 7
  expect_equal(nrow(result$X), 7)
  expect_true("param" %in% names(result$X))
  expect_true(is.data.frame(result$X))
})

test_that("generate_OAT_SA_design keeps non-param columns constant at 1", {
  settings <- make_sa_test_settings()
  sa_samples <- make_mock_sa_samples()
  
  result <- generate_OAT_SA_design(settings, sa_samples = sa_samples)
  
  non_param_cols <- setdiff(names(result$X), "param")
  for (col in non_param_cols) {
    expect_true(all(result$X[[col]] == 1),
      info = paste("Column", col, "should be constant 1 for SA"))
  }
})

test_that("generate_OAT_SA_design param column is sequential", {
  settings <- make_sa_test_settings()
  sa_samples <- make_mock_sa_samples()
  
  result <- generate_OAT_SA_design(settings, sa_samples = sa_samples)
  
  expect_equal(result$X$param, seq_len(nrow(result$X)))
})

test_that("generate_joint_ensemble_design returns correct structure", {
  settings <- make_sa_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))
  
  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
    function(...) list(ids = sample(1:2, 5, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "get.parameter.samples",
    function(...) NULL)
  mockery::stub(generate_joint_ensemble_design, "file.exists",
    function(...) TRUE)
  
  result <- generate_joint_ensemble_design(settings, ensemble_size = 5)
  
  expect_true("X" %in% names(result))
  expect_equal(nrow(result$X), 5)
  expect_true("param" %in% names(result$X))
})

test_that("OAT design has constant inputs while ensemble design varies them", {
  # SA design - all non-param constant
  settings <- make_sa_test_settings()
  sa_samples <- make_mock_sa_samples()
  sa_result <- generate_OAT_SA_design(settings, sa_samples = sa_samples)
  
  sa_non_param <- setdiff(names(sa_result$X), "param")
  for (col in sa_non_param) {
    expect_equal(length(unique(sa_result$X[[col]])), 1,
      info = "SA design: non-param columns must be constant")
  }
  
  # ensemble design - non-param can vary (mocked to show variation)
  settings$run <- list(inputs = list(met = list(path = c("m1.nc", "m2.nc", "m3.nc"))))
  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
    function(...) list(ids = c(1, 2, 3, 1, 2)))
  mockery::stub(generate_joint_ensemble_design, "get.parameter.samples",
    function(...) NULL)
  mockery::stub(generate_joint_ensemble_design, "file.exists",
    function(...) TRUE)
  
  ens_result <- generate_joint_ensemble_design(settings, ensemble_size = 5)
  
  expect_true(length(unique(ens_result$X$met)) > 1,
    info = "Ensemble design: non-param columns should vary")
})