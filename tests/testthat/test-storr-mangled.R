context("storr_mangled")

test_that("simple mangling", {
  st <- storr_rds(tempfile(), default_namespace="foo", mangle_key=TRUE)

  st$set("foo", "bar")
  expect_that(st$get("foo"), equals("bar"))
  expect_that(st$list(), equals(hash_string("foo")))
  expect_that(st$storr$get(hash_string("foo")), equals("bar"))
})

test_that("export", {
  st <- storr_rds(tempfile(), default_namespace="foo", mangle_key=TRUE)
  st$set("foo", mtcars)

  ## Case 1: export all objects:
  path2 <- tempfile()
  st$archive_export(path2)

  st2 <- storr_rds(path2, mangle_key=TRUE)
  expect_that(st2$list("foo"), equals(hash_string("foo")))
  expect_that(st2$get("foo", "foo"), equals(mtcars))

  ## Case 2: export by name.  This one actually requires mangling iff
  ## names=NULL
  path3 <- tempfile()
  st$archive_export(path3, "foo")

  st3 <- storr_rds(path3, mangle_key=TRUE)
  expect_that(st3$list("foo"), equals(hash_string("foo")))
  expect_that(st3$get("foo", "foo"), equals(mtcars))
})
