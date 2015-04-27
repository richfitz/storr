context("export / import")

test_that("export / import", {
  st <- storr_environment()

  st$set("a", mtcars)
  st$set("b", iris)

  st$list()

  path <- tempfile("export_")
  expect_that(dir(path), equals(character(0)))

  st$archive_export(path)

  expect_that(dir(path), equals_unsorted(c("data", "keys", "list")))

  tmp <- storr_rds(path)
  expect_that(tmp$list(), equals(c("a", "b")))
  expect_that(tmp$get("a"), equals(mtcars))

  path2 <- tempfile("export_")
  st$archive_export(path2, c(bar="b"))
  tmp2 <- storr_rds(path2)
  expect_that(tmp2$list(), equals("bar"))
  expect_that(tmp2$get("bar"), equals(iris))

  st2 <- storr_environment()
  expect_that(st2$list(), equals(character(0)))

  st2$archive_import(path)
  expect_that(st2$list(), equals(c("a", "b")))

  st3 <- storr_environment()
  st3$archive_import(path, c(foo="a"))
  expect_that(st3$list(), equals("foo"))
  expect_that(st3$get("foo"), equals(st2$get("a")))
})

test_that("namespace", {
  st <- storr_environment()

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
