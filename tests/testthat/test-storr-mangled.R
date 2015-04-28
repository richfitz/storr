context("storr_mangled")

test_that("simple mangling", {
  st <- storr_rds(tempfile(), default_namespace="foo", mangle_key=TRUE)

  st$set("foo", "bar")
  expect_that(st$get("foo"), equals("bar"))
  expect_that(st$list(), equals(hash_string("foo")))
  expect_that(st$storr$get(hash_string("foo")), equals("bar"))
})
