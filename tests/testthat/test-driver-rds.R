context("driver_rds")

## Tests of the implementation details of the rds driver only...
test_that("creation", {
  path <- tempfile()
  expect_that(file.exists(path), is_false())
  dr <- driver_rds(path)
  on.exit(unlink(path, recursive=TRUE))

  expect_that(file.exists(path), is_true())
  expect_that(dir(path), equals(c("data", "keys")))
  expect_that(dir(file.path(path, "data")), equals(character(0)))
  expect_that(dir(file.path(path, "keys")), equals(character(0)))
})
