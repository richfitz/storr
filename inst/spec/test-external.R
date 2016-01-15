## This requires
##   .driver_name: character(1)
##   .driver_create: function()
testthat::context(sprintf("external [%s]", .driver_name))

testthat::test_that("simple", {
  dr <- .driver_create()
  on.exit(dr$destroy())

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
  testthat::expect_is(st, "storr")
  testthat::expect_is(st, "storr_external")

  testthat::expect_false(st$driver$exists_hash(key, ns))
  testthat::expect_false(st$driver$exists_object(hash))

  testthat::expect_false(st$exists(key, ns))
  testthat::expect_false(st$exists_object(hash))

  testthat::expect_identical(st$list(ns), character(0))

  d <- st$get(key, ns)
  testthat::expect_equal(d, dat, tolerance=1e-15)

  testthat::expect_true(st$driver$exists_hash(key, ns))
  testthat::expect_true(st$driver$exists_object(hash))
  testthat::expect_true(st$exists(key, ns))
  testthat::expect_true(st$exists_object(hash))

  ## We hit the cache on the way in:
  testthat::expect_equal(ls(st$envir), hash)

  ## Second time around will not hit the object:
  testthat::expect_identical(st$get(key, ns), d)
  ## So we can delete the actual remote object:
  file.remove(file.path(path, key))
  testthat::expect_identical(st$get(key, ns), d)

  ## Out of bounds:
  testthat::expect_false(st$exists("z", ns))
  testthat::expect_error(suppressWarnings(st$get("z", ns)),
                         "key 'z' ('objects') not found, with error:",
                         fixed=TRUE)
})
