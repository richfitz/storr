## This requires
##   .driver_name: character(1)
##   .driver_create: function()
context(sprintf("mangling [%s]", .driver_name))

test_that("simple mangling", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr, default_namespace="foo", mangle_key=TRUE)
  st$set("foo", "bar")
  expect_that(st$get("foo"), equals("bar"))
  expect_that(st$list(), equals("foo"))

  st2 <- storr(st$driver)
  expect_that(st2$list("foo"), equals(unclass(mangle("foo"))))
  expect_that(st2$get(mangle("foo"), "foo"), equals("bar"))
})

test_that("export", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr, default_namespace="foo", mangle_key=TRUE)
  st$set("foo", mtcars)

  ## Case 1: export all objects:
  path2 <- tempfile()
  st$archive_export(path2)

  dr2 <- .driver_create()
  on.exit(dr2$destroy(), add=TRUE)
  st2 <- storr(dr2, mangle_key=TRUE)
  ## TODO: this does not import all namespaces, which seems like a bug to me.
  st2$archive_import(path2, namespace="foo")
  ## This has hashed keys on the way into the object which is not
  ## ideal; we have saved the objects as not the right thing.
  expect_that(st2$list("foo"), equals("foo"))
  expect_that(st2$get("foo", "foo"), equals(mtcars))

  ## Can actually connect a storr to this path:
  expect_that(storr_rds(path2, mangle_key=TRUE)$list("foo"), equals("foo"))

  ## Case 2: export by name.
  dr3 <- .driver_create()
  on.exit(dr3$destroy(), add=TRUE)
  st3 <- storr(dr3, mangle_key=TRUE)

  path3 <- tempfile()
  st$archive_export(path3, "foo")

  st3$archive_import(path3, namespace="foo")
  expect_that(st3$list("foo"), equals("foo"))
  expect_that(st3$get("foo", "foo"), equals(mtcars))
})
