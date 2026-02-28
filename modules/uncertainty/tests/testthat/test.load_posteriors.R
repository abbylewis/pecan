# Tests for load.posteriors and related helper functions

# Helper: redirect PEcAn.logger to stdout so expect_output() can capture it
setup_logger_for_testing <- function() {
  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  PEcAn.logger::logger.setLevel("DEBUG")
}

test_that("load from single distns file works", {
  tmpdir <- withr::local_tempdir()

  # Create a post.distns object and save it
  post.distns <- data.frame(
    distn = c("norm", "norm"),
    parama = c(10, 20),
    paramb = c(2, 5),
    row.names = c("trait_a", "trait_b"),
    stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(tmpdir, "my_posteriors.Rdata"))

  result <- load.posteriors(
    posterior.file = file.path(tmpdir, "my_posteriors.Rdata")
  )

  expect_null(result$trait.mcmc)
  expect_false(result$is.pda)
  expect_is(result$prior.distns, "data.frame")
  expect_equal(rownames(result$prior.distns), c("trait_a", "trait_b"))
})


test_that("load from single mcmc file works", {
  tmpdir <- withr::local_tempdir()

  # Create a trait.mcmc object (list of matrices, mimicking coda output)
  trait.mcmc <- list(
    trait_a = list(matrix(rnorm(100), ncol = 1, dimnames = list(NULL, "beta.o"))),
    trait_b = list(matrix(rnorm(100), ncol = 1, dimnames = list(NULL, "beta.o")))
  )
  save(trait.mcmc, file = file.path(tmpdir, "samples.Rdata"))

  result <- load.posteriors(
    posterior.file = file.path(tmpdir, "samples.Rdata")
  )

  expect_is(result$trait.mcmc, "list")
  expect_equal(names(result$trait.mcmc), c("trait_a", "trait_b"))
  expect_null(result$prior.distns)
  expect_false(result$is.pda)
})


test_that("load from directory with both mcmc and distns prefers mcmc", {
  tmpdir <- withr::local_tempdir()

  # File 1: distribution summaries
  post.distns <- data.frame(
    distn = c("norm", "norm"),
    parama = c(10, 20),
    paramb = c(2, 5),
    row.names = c("trait_a", "trait_b"),
    stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(tmpdir, "distributions.Rdata"))

  # File 2: MCMC samples
  trait.mcmc <- list(
    trait_a = list(matrix(rnorm(100), ncol = 1, dimnames = list(NULL, "beta.o")))
  )
  save(trait.mcmc, file = file.path(tmpdir, "mcmc_samples.Rdata"))

  result <- load.posteriors(posterior.file = tmpdir)

  # Both should be loaded

  expect_is(result$trait.mcmc, "list")
  expect_equal(names(result$trait.mcmc), c("trait_a"))
  expect_is(result$prior.distns, "data.frame")
  expect_false(result$is.pda)
})


test_that("load from directory with only distns works", {
  tmpdir <- withr::local_tempdir()

  post.distns <- data.frame(
    distn = "norm",
    parama = 10,
    paramb = 2,
    row.names = "trait_a",
    stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(tmpdir, "post.Rdata"))

  result <- load.posteriors(posterior.file = tmpdir)

  expect_null(result$trait.mcmc)
  expect_is(result$prior.distns, "data.frame")
  expect_equal(rownames(result$prior.distns), "trait_a")
})


test_that("load from directory with only mcmc works", {
  tmpdir <- withr::local_tempdir()

  trait.mcmc <- list(
    trait_a = list(matrix(rnorm(100), ncol = 1, dimnames = list(NULL, "beta.o")))
  )
  save(trait.mcmc, file = file.path(tmpdir, "mcmc.Rdata"))

  result <- load.posteriors(posterior.file = tmpdir)

  expect_is(result$trait.mcmc, "list")
  expect_null(result$prior.distns)
})


test_that("fallback to outdir emits deprecation warning", {
  tmpdir <- withr::local_tempdir()
  setup_logger_for_testing()
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

  # Create prior.distns in outdir
  prior.distns <- data.frame(
    distn = "norm",
    parama = 10,
    paramb = 2,
    row.names = "trait_a",
    stringsAsFactors = FALSE
  )
  save(prior.distns, file = file.path(tmpdir, "prior.distns.Rdata"))

  expect_output(
    result <- load.posteriors(
      posterior.file = NA,
      outdir = tmpdir
    ),
    "deprecated"
  )

  expect_is(result$prior.distns, "data.frame")
  expect_equal(rownames(result$prior.distns), "trait_a")
})


test_that("fallback to outdir loads post.distns over prior.distns", {
  tmpdir <- withr::local_tempdir()

  # Create both post.distns and prior.distns
  post.distns <- data.frame(
    distn = "norm",
    parama = 99,
    paramb = 1,
    row.names = "trait_post",
    stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(tmpdir, "post.distns.Rdata"))

  prior.distns <- data.frame(
    distn = "norm",
    parama = 10,
    paramb = 2,
    row.names = "trait_prior",
    stringsAsFactors = FALSE
  )
  save(prior.distns, file = file.path(tmpdir, "prior.distns.Rdata"))

  invisible(capture.output(
    result <- load.posteriors(
      posterior.file = NA,
      outdir = tmpdir
    )
  ))

  # Should get post.distns, not prior.distns
  expect_equal(rownames(result$prior.distns), "trait_post")
})


test_that("fallback outdir finds mcmc in directory scan", {
  tmpdir <- withr::local_tempdir()

  # Create MCMC file with non-standard name
  trait.mcmc <- list(
    trait_a = list(matrix(rnorm(100), ncol = 1, dimnames = list(NULL, "beta.o")))
  )
  save(trait.mcmc, file = file.path(tmpdir, "my_custom_mcmc.Rdata"))

  invisible(capture.output(
    result <- load.posteriors(
      posterior.file = NA,
      outdir = tmpdir
    )
  ))

  expect_is(result$trait.mcmc, "list")
  expect_equal(names(result$trait.mcmc), "trait_a")
})


test_that("prior.distns file accepted when post.distns absent", {
  tmpdir <- withr::local_tempdir()

  prior.distns <- data.frame(
    distn = "norm",
    parama = 5,
    paramb = 1,
    row.names = "my_trait",
    stringsAsFactors = FALSE
  )
  save(prior.distns, file = file.path(tmpdir, "priors.Rdata"))

  result <- load.posteriors(posterior.file = file.path(tmpdir, "priors.Rdata"))

  expect_is(result$prior.distns, "data.frame")
  expect_equal(rownames(result$prior.distns), "my_trait")
  expect_null(result$trait.mcmc)
})


test_that("nonexistent path returns empty result with error", {
  tmpdir <- withr::local_tempdir()
  fake_path <- file.path(tmpdir, "does_not_exist.Rdata")
  setup_logger_for_testing()
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

  expect_output(
    result <- load.posteriors(
      posterior.file = fake_path
    ),
    "does not exist"
  )

  expect_null(result$prior.distns)
  expect_null(result$trait.mcmc)
  expect_false(result$is.pda)
})


test_that("empty directory returns empty result", {
  tmpdir <- withr::local_tempdir()
  setup_logger_for_testing()
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

  expect_output(
    result <- load.posteriors(posterior.file = tmpdir),
    "No .Rdata files found"
  )

  expect_null(result$prior.distns)
  expect_null(result$trait.mcmc)
})


test_that("PDA detection works via filename heuristic in legacy path", {
  tmpdir <- withr::local_tempdir()

  # Create mcmc file with pda in name
  trait.mcmc <- list(
    trait_a = list(matrix(rnorm(100), ncol = 1, dimnames = list(NULL, "beta.o")))
  )
  save(trait.mcmc, file = file.path(tmpdir, "mcmc.pda.Rdata"))

  invisible(capture.output(
    result <- load.posteriors(
      posterior.file = NA,
      outdir = tmpdir
    )
  ))

  expect_true(result$is.pda)
  expect_is(result$trait.mcmc, "list")
})
