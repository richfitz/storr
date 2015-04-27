context("environments")

test_that("export", {
  path1 <- tempfile()
  cache <- storr(driver_rds(path1))
  on.exit(unlink(path1, recursive=TRUE))

  path2 <- tempfile()
  cache2 <- storr(driver_rds(path2))
  on.exit(unlink(path2, recursive=TRUE))

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
  path1 <- tempfile()
  cache <- storr_rds(path1)
  on.exit(unlink(path1, recursive=TRUE))

  path2 <- tempfile()
  cache2 <- storr_rds(path2)
  on.exit(unlink(path2, recursive=TRUE))

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
