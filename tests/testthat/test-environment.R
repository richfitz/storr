context("environments")

test_that("export", {
  path <- tempdir()
  cache <- object_cache(driver_rds(path))
  on.exit(unlink(path, recursive=TRUE))

  ## Need a function to generate a bunch of objects
  cache$set("d", mtcars)
  e <- cache$to_environment()
  expect_that(ls(e), equals("d"))
  expect_that(e[["d"]], equals(mtcars))

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
