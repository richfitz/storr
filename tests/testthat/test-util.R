context("utils")

test_that("write_serialized_rds", {
  x <- runif(100)
  sx <- serialize(x, NULL)

  p1 <- tempfile()
  write_serialized_rds(sx, p1, FALSE)
  on.exit(file.remove(p1), add = TRUE)
  expect_identical(readBin(p1, raw(), length(sx) * 2), sx)
  expect_identical(readRDS(p1), x)

  p2 <- tempfile()
  write_serialized_rds(sx, p2, FALSE, length(sx) / 2)
  on.exit(file.remove(p2), add = TRUE)
  expect_identical(readBin(p2, raw(), length(sx) * 2), sx)
  expect_identical(readRDS(p2), x)

  expect_equal(unname(tools::md5sum(p1)),
               unname(tools::md5sum(p2)))
})

test_that("assertions", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")

  expect_error(assert_length(1, 2), "must have 2 elements")

  expect_error(assert_function("sin"), "must be a function")

  expect_error(assert_environment("foo"), "must be an environment")

  expect_error(assert_list(1:5), "must be a list")
  expect_error(assert_logical(1:5), "must be logical")
  expect_error(assert_character(1:5), "must be character")
  expect_error(assert_raw(1:5), "must be raw")
})
