context("spec")

test_that("reporter handling", {
  create <- function(dr = NULL, ...) driver_environment(dr$envir, ...)
  expect_message(r <- testthat::with_reporter(
    testthat::StopReporter,
    ans <- test_driver(create)),
    "PASS")
  expect_is(r, "StopReporter")
  expect_is(ans, "data.frame")
})
