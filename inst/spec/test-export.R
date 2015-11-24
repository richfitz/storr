## This requires
##   .driver_name: character(1)
##   .driver_create: function()
context(sprintf("export [%s]", .driver_name))

## TODO: not tested properly; *importing* from a cache; the inverse of
## this with dr being the recipient and environment being the source.

test_that("export", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  cache <- storr(dr)

  ## This could be any old thing, but for now we'll use an environment storr:
  cache2 <- storr(driver_environment())

  ## Need a function to generate a bunch of objects
  cache$set("d", mtcars)
  e <- cache$to_environment()
  expect_that(ls(e), equals("d"))
  expect_that(e[["d"]], equals(mtcars))

  cache$export(cache2)
  expect_that(cache2$list(), equals("d"))
  expect_that(cache2$get("d"), equals(mtcars))

  e$dat <- iris
  nms <- cache$import(e)
  expect_that(nms, equals(c("d", "dat")))
  expect_that(cache$get("dat"), equals(iris))

  env <- new.env(parent=emptyenv())
  nms <- cache$export(env)
  expect_that(nms, equals(c("d", "dat")))
  expect_that(env$d, equals(mtcars))
  expect_that(env$dat, equals(iris))
})

test_that("namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  cache <- storr(dr)

  cache2 <- storr(driver_environment())

  cache$set("d", mtcars, namespace="ns1")
  cache$set("d", iris,  namespace="ns2")

  cache$export(cache2)
  expect_that(cache2$list(), equals(character(0)))

  cache$export(cache2, namespace="ns1")
  expect_that(cache2$list("ns1"), equals("d"))
  expect_that(cache2$get("d", "ns1"), equals(mtcars))

  cache$export(cache2, namespace="ns2")
  expect_that(cache2$list("ns2"), equals("d"))
  expect_that(cache2$get("d", "ns2"), equals(iris))
})

test_that("import / export", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)
  st$set("a", mtcars)
  st$set("b", iris)
  expect_that(st$list(), equals(c("a", "b")))

  path <- tempfile("export_")
  expect_that(dir(path), equals(character(0)))
  st$archive_export(path)
  expect_that(dir(path), equals_unsorted(c("data", "keys", "list")))

  ## Load into an rds storr:
  tmp <- storr_rds(path)
  expect_that(tmp$list(), equals(c("a", "b")))
  expect_that(tmp$get("a"), equals(mtcars))

  path2 <- tempfile("export_")
  st$archive_export(path2, c(bar="b"))
  tmp2 <- storr_rds(path2)
  expect_that(tmp2$list(), equals("bar"))
  expect_that(tmp2$get("bar"), equals(iris))

  dr2 <- .driver_create()
  on.exit(dr2$destroy(), add=TRUE)
  st2 <- storr(dr2)
  expect_that(st2$list(), equals(character(0)))

  st2$archive_import(path)
  expect_that(st2$list(), equals(c("a", "b")))

  dr3 <- .driver_create()
  on.exit(dr3$destroy(), add=TRUE)
  st3 <- storr(dr3)

  st3$archive_import(path, c(foo="a"))
  expect_that(st3$list(), equals("foo"))
  expect_that(st3$get("foo"), equals(st2$get("a")))
})

test_that("namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  st$set("a", mtcars, namespace="ns1")
  st$set("b", iris,   namespace="ns2")

  path <- tempfile("export_")
  expect_that(dir(path), equals(character(0)))

  tmp <- storr_rds(path)

  st$archive_export(path)
  expect_that(tmp$list(), equals(character(0)))

  st$archive_export(path, namespace="ns1")
  expect_that(tmp$list(), equals(character(0)))
  expect_that(tmp$list("ns1"), equals("a"))

  st$archive_export(path, namespace="ns2")
  expect_that(tmp$list(), equals(character(0)))
  expect_that(tmp$list("ns2"), equals("b"))
})
