context("spec")

test_that("reporter handling", {
  skip_on_cran()
  create <- function(dr = NULL, ...) driver_environment(dr$envir, ...)
  res <- testthat::evaluate_promise(
    testthat::with_reporter(
      testthat::StopReporter,
      ans <- test_driver(create)))

  expect_is(res$result, "StopReporter")
  expect_match(res$messages, "PASS")
  expect_match(res$output, "DONE")
  expect_is(ans, "data.frame")

  expect_silent(test_driver_finish(ans, FALSE))
  expect_message(test_driver_finish(ans, TRUE),
                 "PASS: 0 errors, 0 failures / [0-9]+ tests total")

  ans$error[[1]] <- TRUE
  ans$failed[[1]] <- 2
  expect_silent(test_driver_finish(ans, FALSE))
  expect_error(test_driver_finish(ans, TRUE),
               "FAIL: 1 error, 2 failures", fixed = TRUE)
})
