context("driver rds details")

## Tests of the implementation details of the rds driver only...
test_that("creation", {
  path <- tempfile()
  expect_false(file.exists(path))
  dr <- driver_rds(path)
  on.exit(dr$destroy())

  expect_true(file.exists(path))
  expect_identical(sort(dir(path)), c("data", "keys"))
  expect_identical(dir(file.path(path, "data")), character(0))
})
