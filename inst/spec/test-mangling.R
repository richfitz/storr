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
  expect_that(st$list(), equals(hash_string("foo")))
  expect_that(st$storr$get(hash_string("foo")), equals("bar"))
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
  expect_that(st2$list("foo"), equals(hash_string("foo")))
  expect_that(st2$get("foo", "foo"), equals(mtcars))

  ## Case 2: export by name.  This one actually requires mangling iff
  ## names=NULL
  dr3 <- .driver_create()
  on.exit(dr3$destroy(), add=TRUE)
  st3 <- storr(dr3, mangle_key=TRUE)

  path3 <- tempfile()
  st$archive_export(path3, "foo")

  st3$archive_import(path3, namespace="foo")
  expect_that(st3$list("foo"), equals(hash_string("foo")))
  expect_that(st3$get("foo", "foo"), equals(mtcars))
})
