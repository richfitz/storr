context("driver rds details")

## Tests of the implementation details of the rds driver only...
test_that("creation", {
  path <- tempfile()
  expect_that(file.exists(path), is_false())
  dr <- driver_rds(path)
  expect_that(dr$path, equals(path))
  on.exit(dr$destroy())

  expect_that(file.exists(path), is_true())
  expect_that(dir(path), equals_unsorted(c("data", "keys", "list")))
  expect_that(dir(file.path(path, "data")), equals(character(0)))
})

test_that("storr_rds", {
  path <- tempfile()
  st <- storr_rds(tempfile())
})
