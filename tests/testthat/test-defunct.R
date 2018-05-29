context("defunct")

test_that("defunct functions", {
  expect_error(driver_redis_api(), "redux::driver_redis_api", fixed = TRUE)
  expect_error(storr_redis_api(), "redux::storr_redis_api", fixed = TRUE)
})
