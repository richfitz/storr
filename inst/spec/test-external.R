## This requires
##   .driver_name: character(1)
##   .driver_create: function()
context(sprintf("external [%s]", .driver_name))

test_that("simple", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  ## Set up some data:
  path <- tempfile()
  on.exit(unlink(path, recursive=TRUE), add=TRUE)

  ## Set up some data:
  dat <- "aaa"
  key <- "a"
  hash <- hash_object(dat)
  ns <- "objects"

  dir.create(path)
  writeLines(dat, file.path(path, key))

  f_fetch <- function(key, namespace) {
    file.path(path, key)
  }

  st <- storr_external(dr, fetch_hook_read(f_fetch, readLines))
  expect_that(st, is_a("storr"))
  expect_that(st, is_a("storr_external"))

  expect_that(st$driver$exists_key(key, ns), is_false())
  expect_that(st$driver$exists_hash(hash), is_false())

  expect_that(st$exists(key, ns), is_false())
  expect_that(st$exists_hash(hash), is_false())

  expect_that(st$list(ns), equals(character(0)))

  d <- st$get(key, ns)
  expect_that(d, equals(dat))

  expect_that(st$driver$exists_key(key, ns), is_true())
  expect_that(st$driver$exists_hash(hash), is_true())
  expect_that(st$exists(key, ns), is_true())
  expect_that(st$exists_hash(hash), is_true())

  ## Out of bounds:
  expect_that(st$exists("z", ns), is_false())
  expect_that(suppressWarnings(st$get("z", ns)),
              throws_error("key 'z' ('objects') not found, with error:",
                           fixed=TRUE))
})

test_that("storr", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  path <- tempfile()
  on.exit(unlink(path, recursive=TRUE), add=TRUE)

  ## Set up some data:
  dat <- "aaa"
  key <- "a"
  hash <- hash_object(dat)

  dir.create(path)
  writeLines(dat, file.path(path, key))

  f_fetch <- function(key, namespace) {
    file.path(path, key)
  }

  st <- storr_external(dr, fetch_hook_read(f_fetch, readLines))
  expect_that(st$list(), equals(character(0)))
  tmp <- st$get(key)
  expect_that(tmp, is_identical_to(dat))

  expect_that(suppressWarnings(st$get("z")),
              throws_error("not found, with error"))
})
