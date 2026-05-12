
test_that("child gets parent ids", {
  s <- list()
  s$run$inputs$met$path <- paste0(1:50, ".clim")

  parent_idx <- sample(20, replace = TRUE)
  child_idx <- input.ens.gen(s, ensemble_size = 20,
                             input = "met", parent_ids = parent_idx)

  expect_equal(parent_idx, child_idx$ids)
})

test_that("parent ids accepted as vector or as list for compatibility", {
  s <- list()
  s$run$inputs$soil$path <- paste0(1:20, ".csv")

  expect_identical(
    input.ens.gen(s, ensemble_size = 20, input = "soil", parent_ids = 1:20),
    input.ens.gen(s, ensemble_size = 20, input = "soil",
                  parent_ids = list(ids = 1:20))
  )
})

test_that("complains on length mismatch", {
  s <- list()
  s$run$inputs$soil$path <- paste0(1:20, ".csv")

  res <- capture.output(
    input.ens.gen(s, ensemble_size = 10, input = "soil", parent_ids = 1:3),
    type = "message"
  )

  expect_match(res, "same length as the ensemble", all = FALSE)
})


# This is legacy behavior that we may want to remove!
# But meanwhile it was implemented on purpose and we shouldn't break it
# accidentally.
test_that("takes samples to replace invalid parent indices", {
  s <- list()
  s$run$inputs$a$path <- paste0(1:10, ".nc")

  res <- input.ens.gen(s, ensemble_size = 10, input = "a", parent_ids = 6:15)
  expect_equal(res$ids[1:5], 6:10)
  expect_true(all(res$ids[6:10] %in% 1:10))

})

test_that("parent ids used even when no sampling method given", {
  s <- list()
  s$run$inputs$met$path <- paste0(1:10, ".nc")

  parents <- c(1, 7, 5, 2)

  for (method in list("sampling", "looping", NULL)) {
    res <- input.ens.gen(s,
                         ensemble_size = 4,
                         input = "met",
                         method = method,
                         parent_ids = parents)
    expect_equal(res$ids, parents)
  }
})

