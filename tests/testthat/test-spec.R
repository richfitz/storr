context("spec")

test_that("reporter handling", {
  create <- function(dr = NULL, ...) driver_environment(dr$envir, ...)
  res <- testthat::evaluate_promise(
    testthat::with_reporter(
      testthat::StopReporter,
      ans <- test_driver(create)))

  expect_is(res$result, "StopReporter")
  expect_match(res$messages, "PASS")
  expect_match(res$output, "DONE")
  expect_is(ans, "data.frame")
})
