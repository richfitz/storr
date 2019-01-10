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
  write_serialized_rds(sx, p2, FALSE, long = length(sx) / 2)
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

  expect_error(assert_is(TRUE, "foo"), "'TRUE' must be a foo")
})

test_that("write_serialized_rds recovers on error", {
  skip_if_not_installed("mockr")
  value <- serialize(1:100, NULL)
  filename <- tempfile()

  partial_failure <- function(object, con, ...) {
    writeLines("somedata", con)
    stop("Error writing to disk")
  }
  total_failure <- function(object, con, ...) {
    stop("Error writing to disk")
  }

  mockr::with_mock(
    try_write_serialized_rds = partial_failure,
    expect_error(write_serialized_rds(value, filename, FALSE),
                 "Error writing to disk"))
  expect_false(file.exists(filename))

  mockr::with_mock(
    try_write_serialized_rds = total_failure,
    expect_error(write_serialized_rds(value, filename, FALSE),
                 "Error writing to disk"))
  expect_false(file.exists(filename))

  expect_silent(write_serialized_rds(value, filename, FALSE))
  expect_identical(readRDS(filename), unserialize(value))
})

test_that("write_lines recovers on error", {
  skip_if_not_installed("mockr")
  value <- "hello"
  filename <- tempfile()

  partial_failure <- function(object, con, ...) {
    writeLines("corrupt", con)
    stop("Error writing to disk")
  }
  total_failure <- function(object, con, ...) {
    stop("Error writing to disk")
  }

  mockr::with_mock(
    try_write_lines = partial_failure,
    expect_error(write_lines(value, filename),
                 "Error writing to disk"))
  expect_false(file.exists(filename))

  mockr::with_mock(
    try_write_lines = total_failure,
    expect_error(write_lines(value, filename),
                 "Error writing to disk"))
  expect_false(file.exists(filename))

  expect_silent(write_lines(value, filename))
  expect_identical(readLines(filename), value)
})


test_that("read_text_file throws on missing file", {
  expect_error(
    read_text_file(tempfile(), 12),
    "File '.+' does not exist")
})


test_that("read_text_file throws on empty file", {
  path <- tempfile()
  file.create(path)
  on.exit(unlink(path))
  expect_error(read_text_file(path, 12),
               "File '.+' is empty")
})


test_that("read_text_file behaves sensibly on multiline files", {
  path <- tempfile()
  writeLines(c("123", "4567"), path)
  on.exit(unlink(path))

  expect_equal(readLines(path), c("123", "4567"))
  expect_equal(read_text_file(path, 3), "123")
  oneline <- read_text_file(path, 100) # Includes '\n'.
  expect_match(oneline, "^123\\s+$")
})


test_that("read_text_file bulk test", {
  test_read_text_file <- function(n) {
     string <- rand_str(n, FALSE)
     file <- tempfile()
     writeLines(string, file)
     on.exit(unlink(file))
     read_text_file(file, n) == string
  }
  expect_true(all(vlapply(seq_len(128), test_read_text_file)))
})


test_that("read_rds throws on missing file", {
  expect_error(read_rds(tempfile()),
               "rds file '.+' missing")
})


test_that("read_rds reads rds files", {
  path <- tempfile()
  x <- runif(10)
  saveRDS(x, path)
  on.exit(unlink(path))
  expect_identical(read_rds(path), x)
})
