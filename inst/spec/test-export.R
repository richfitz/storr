## This requires
##   .driver_name: character(1)
##   .driver_create: function()
testthat::context(sprintf("export [%s]", .driver_name))

## TODO: not tested properly; *importing* from a cache; the inverse of
## this with dr being the recipient and environment being the source.

testthat::test_that("export", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  cache <- storr(dr)

  ## This could be any old thing, but for now we'll use an environment storr:
  cache2 <- storr(driver_environment())

  ## Need a function to generate a bunch of objects
  cache$set("d", mtcars)
  e <- cache$export(new.env())
  testthat::expect_identical(ls(e), "d")
  testthat::expect_equal(e[["d"]], mtcars)

  cache$export(cache2)
  testthat::expect_identical(cache2$list(), "d")
  testthat::expect_equal(cache2$get("d"), mtcars)

  e$dat <- iris
  nms <- cache$import(e)
  testthat::expect_identical(nms, c("d", "dat"))
  testthat::expect_equal(cache$get("dat"), iris)

  env <- cache$export(new.env(parent=emptyenv()))
  testthat::expect_identical(ls(e), c("d", "dat"))
  testthat::expect_equal(env$d, mtcars)
  testthat::expect_equal(env$dat, iris)
})

## This is pretty minimal:
testthat::test_that("import", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  cache <- storr(dr)

  cache2 <- storr(driver_environment())
  cache2$set("d", mtcars)

  cache2$export(cache)
  testthat::expect_identical(cache$list(), "d")
  testthat::expect_equal(cache$get("d"), mtcars)
})

testthat::test_that("namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  cache <- storr(dr)

  cache2 <- storr(driver_environment())

  cache$set("d", mtcars, namespace="ns1")
  cache$set("d", iris,  namespace="ns2")

  cache$export(cache2)
  testthat::expect_identical(cache2$list(), character(0))

  cache$export(cache2, namespace="ns1")
  testthat::expect_identical(cache2$list("ns1"), "d")
  testthat::expect_equal(cache2$get("d", "ns1"), mtcars)

  cache$export(cache2, namespace="ns2")
  testthat::expect_identical(cache2$list("ns2"), "d")
  testthat::expect_equal(cache2$get("d", "ns2"), iris)
})

testthat::test_that("import / export", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)
  st$set("a", mtcars)
  st$set("b", iris)
  testthat::expect_identical(st$list(), c("a", "b"))

  path <- tempfile("export_")
  on.exit(unlink(path, recursive=TRUE), add=TRUE)
  testthat::expect_identical(dir(path), character(0))
  st$archive_export(path)
  testthat::expect_identical(sort(dir(path)), c("config", "data", "keys"))

  ## Load into an rds storr:
  tmp <- storr_rds(path, mangle_key=TRUE)
  testthat::expect_identical(sort(tmp$list()), c("a", "b"))
  testthat::expect_equal(tmp$get("a"), mtcars, tolerance=1e-15)

  path2 <- tempfile("export_")
  on.exit(unlink(path2, recursive=TRUE), add=TRUE)
  st$archive_export(path2, c(bar="b"))
  tmp2 <- storr_rds(path2, mangle_key=TRUE)
  testthat::expect_identical(tmp2$list(), "bar")
  testthat::expect_equal(tmp2$get("bar"), iris, tolerance=1e-15)

  dr$destroy()
  dr <- .driver_create()
  st <- storr(dr)
  testthat::expect_identical(st$list(), character(0))

  st$archive_import(path)
  testthat::expect_identical(sort(st$list()), c("a", "b"))

  dr$destroy()
  dr <- .driver_create()
  st <- storr(dr)

  st$archive_import(path, c(foo="a"))
  testthat::expect_identical(st$list(), "foo")
  testthat::expect_identical(st$get("foo"), tmp$get("a"))
})

testthat::test_that("namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  st$set("a", mtcars, namespace="ns1")
  st$set("b", iris,   namespace="ns2")

  path <- tempfile("export_")
  on.exit(unlink(path, recursive=TRUE), add=TRUE)
  testthat::expect_identical(dir(path), character(0))

  tmp <- storr_rds(path, mangle_key=TRUE)

  st$archive_export(path)
  testthat::expect_identical(tmp$list(), character(0))

  st$archive_export(path, namespace="ns1")
  testthat::expect_identical(tmp$list(), character(0))
  testthat::expect_identical(tmp$list("ns1"), "a")

  st$archive_export(path, namespace="ns2")
  testthat::expect_identical(tmp$list(), character(0))
  testthat::expect_identical(tmp$list("ns2"), "b")
})

testthat::test_that("export list", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  vals <- runif(10)
  st$set("foo", vals)
  x <- st$export(list())
  testthat::expect_equal(x, list(foo=vals), tolerance=1e-15)
  testthat::expect_is(x, "list")

  y <- st$export(list(foo=1, bar=2))
  testthat::expect_equal(y, list(foo=vals, bar=2), tolerance=1e-15)
})

testthat::test_that("export environment", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  vals <- runif(10)
  st$set("foo", vals)
  x <- st$export(new.env(parent=emptyenv()))
  testthat::expect_is(x, "environment")
  testthat::expect_identical(ls(x), "foo")
  testthat::expect_equal(x$foo, vals, tolerance=1e-15)

  y <- st$export(list2env(list(foo=1, bar=2), parent=emptyenv()))
  testthat::expect_identical(ls(y), c("bar", "foo"))
  testthat::expect_equal(y$foo, vals, tolerance=1e-15)
  testthat::expect_identical(y$bar, 2)
})
