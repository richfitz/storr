context("utils")

test_that("write_bin", {
  bytes <- as.raw(0:255)
  path <- tempfile()

  con <- file(path, "wb")
  on.exit({
    close(con)
    file.remove(path)
  })

  write_bin(bytes, con, 7L)

  close(con)
  on.exit(file.remove(path))

  expect_identical(readBin(path, raw(), 1000), bytes)
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
