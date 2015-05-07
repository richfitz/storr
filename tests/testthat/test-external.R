context("driver_external")

test_that("simple", {
  ## Set up some data:
  path <- tempfile()

  dat <- "aaa"
  key <- "a"
  hash <- hash_object(dat)
  ns <- "objects"

  dir.create(path)
  writeLines(dat, file.path(path, key))

  f_read <- readLines
  f_fetch <- function(key, namespace) {
    file.path(path, key)
  }

  d <- driver_rds(tempfile())
  dd <- driver_external(d, fetch_hook_read(f_fetch, readLines))

  expect_that(dd$exists_key(key, ns), is_false())
  expect_that(dd$exists_hash(hash), is_false())

  tmp <- dd$get_hash(key, ns)
  expect_that(tmp, equals(hash))
  expect_that(dd$exists_hash(hash), is_true())
  expect_that(dd$exists_key(key, ns), is_true())
  expect_that(dd$get_value(hash), equals(dat))

  ## Out of bounds:
  expect_that(dd$exists_key("z", ns), is_false())
  expect_that(suppressWarnings(dd$get_hash("z", ns)),
              throws_error("key 'z' not found, with error:"))
})

test_that("download", {
  fmt <- "https://raw.githubusercontent.com/%s/master/DESCRIPTION"

  d <- driver_environment()
  dd <- driver_external(d, fetch_hook_download_fmt(fmt, read.dcf))

  key <- "richfitz/storr"
  ns <- "objects"
  expect_that(dd$exists_key(key, ns), is_false())

  skip_if_no_downloads()
  tmp <- dd$get_hash(key, ns)
  expect_that(dd$exists_key(key, ns), is_true())
  expect_that(dd$get_hash(key, ns), equals(tmp))

  dat <- dd$get_value(tmp)
  expect_that(dat, is_a("matrix"))
  expect_that(unname(dat[, "Package"]), equals("storr"))

  expect_that(dd$get_hash("xxx", ns),
              throws_error("key 'xxx' not found, with error"))
})
