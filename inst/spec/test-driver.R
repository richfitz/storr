## This requires
##   .driver_name: character(1)
##   .driver_create: function()
context(sprintf("drivers [%s]", .driver_name))

test_that("basic (empty)", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  expect_that(dr$type(), equals(.driver_name))

  ns <- "objects"
  expect_that(dr$list_namespaces(), equals(character(0)))
  expect_that(dr$list_hashes(), equals(character(0)))
  expect_that(dr$list_keys(ns), equals(character(0)))

  h <- "aaa"
  k <- "bbb"
  ns <- "ns"

  expect_that(dr$exists_hash(h), is_false())
  expect_that(dr$exists_key(k, ns),  is_false())

  expect_that(dr$del_hash(h), is_false())
  expect_that(dr$del_key(h, ns),  is_false())
})

test_that("set", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  ## First, let's set some data to a hash
  d <- runif(100)
  h <- hash_object(d)
  k <- "bbb"
  ns <- "ns"

  dr$set_object(h, d)
  expect_that(dr$get_object(h), equals(d, tolerance=1e-15))

  ## Then, set a key to address that hash:
  dr$set_hash(k, ns, h)
  expect_that(dr$get_hash(k, ns), is_identical_to(h))

  expect_that(dr$exists_hash(h), is_true())
  expect_that(dr$exists_key(k, ns), is_true())

  ## Then delete the key:
  expect_that(dr$del_key(k, ns), is_true())
  expect_that(dr$del_key(k, ns), is_false())
  expect_that(dr$exists_key(k, ns), is_false())

  ## And delete the hash:
  expect_that(dr$del_hash(h), is_true())
  expect_that(dr$del_hash(h), is_false())
  expect_that(dr$exists_hash(h), is_false())
})

test_that("namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  ## First, let's set some data to a hash
  d <- runif(100)
  h <- hash_object(d)
  k <- "bbb"
  ns <- "ns"

  dr$set_object(h, d)
  expect_that(dr$get_object(h), equals(d, tolerance=1e-15))
  expect_that(dr$exists_hash(h), is_true())

  dr$set_hash(k, ns, h)
  expect_that(dr$get_hash(k, ns), is_identical_to(h))

  expect_that(dr$list_keys(ns), equals(k))
  expect_that(dr$exists_key(k, ns), is_true())

  expect_that(dr$del_key(k, ns), is_true())
  expect_that(dr$exists_key(k, ns), is_false())
  expect_that(dr$exists_hash(h), is_true())

  ## Save into a different namespace:
  ns2 <- "another"
  k2 <- "ccc"

  dr$set_hash(k2, ns2, h)
  expect_that(dr$exists_key(k2, ns2), is_true())
  expect_that(dr$get_hash(k2, ns2), is_identical_to(h))

  expect_that(dr$list_keys(ns), equals(character(0)))
  expect_that(dr$list_keys(ns2), equals("ccc"))
  expect_that(dr$list_hashes(), equals(h))

  expect_that(dr$del_key(k2, ns2), is_true())
  expect_that(dr$exists_key(k2, ns2), is_false())
})
