## This requires
##   .driver_name: character(1)
##   .driver_create: function()
context(sprintf("external [%s]", .driver_name))

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

  dr <- .driver_create()
  on.exit(dr$destroy())

  dd <- driver_external(dr, fetch_hook_read(f_fetch, readLines))

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
