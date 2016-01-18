## TODO: check that *updating* a key works in the spec tests (i.e., hit set_hash(key, namespace, hash)
## This requires
##   .driver_name: character(1)
##   .driver_create: function()
testthat::context(sprintf("drivers [%s]", .driver_name))

testthat::test_that("basic (empty)", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  testthat::expect_equal(dr$type(), .driver_name)

  ns <- "objects"
  testthat::expect_equal(dr$list_namespaces(), character(0))
  testthat::expect_equal(dr$list_hashes(), character(0))
  testthat::expect_equal(dr$list_keys(ns), character(0))

  h <- "aaa"
  k <- "bbb"
  ns <- "ns"

  testthat::expect_false(dr$exists_object(h))
  testthat::expect_false(dr$exists_hash(k, ns))

  testthat::expect_false(dr$del_object(h))
  testthat::expect_false(dr$del_hash(h, ns))
})

testthat::test_that("set", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  ## First, let's set some data to a hash
  d <- runif(100)
  h <- hash_object(d)
  k <- "bbb"
  ns <- "ns"

  if (isTRUE(dr$traits[["accept_raw"]])) {
    dr$set_object(h, serialize(d, NULL))
  } else {
    dr$set_object(h, d)
  }
  testthat::expect_equal(dr$get_object(h), d, tolerance=1e-15)

  ## Then, set a key to address that hash:
  dr$set_hash(k, ns, h)
  testthat::expect_identical(dr$get_hash(k, ns), h)

  ## Check that *updating* a hash works.
  h2 <- hash_object(h)
  dr$set_hash(k, ns, h2)
  testthat::expect_identical(dr$get_hash(k, ns), h2)

  testthat::expect_true(dr$exists_object(h))
  testthat::expect_true(dr$exists_hash(k, ns))

  ## Then delete the key:
  testthat::expect_true(dr$del_hash(k, ns))
  testthat::expect_false(dr$del_hash(k, ns))
  testthat::expect_false(dr$exists_hash(k, ns))

  ## And delete the hash:
  testthat::expect_true(dr$del_object(h))
  testthat::expect_false(dr$del_object(h))
  testthat::expect_false(dr$exists_object(h))
})

testthat::test_that("namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  ## First, let's set some data to a hash
  d <- runif(100)
  h <- hash_object(d)
  k <- "bbb"
  ns <- "ns"

  if (isTRUE(dr$traits[["accept_raw"]])) {
    dr$set_object(h, serialize(d, NULL))
  } else {
    dr$set_object(h, d)
  }
  testthat::expect_equal(dr$get_object(h), d, tolerance=1e-15)
  testthat::expect_true(dr$exists_object(h))

  dr$set_hash(k, ns, h)
  testthat::expect_identical(dr$get_hash(k, ns), h)

  testthat::expect_identical(dr$list_keys(ns), k)
  testthat::expect_true(dr$exists_hash(k, ns))

  testthat::expect_true(dr$del_hash(k, ns))
  testthat::expect_false(dr$exists_hash(k, ns))
  testthat::expect_true(dr$exists_object(h))

  ## Save into a different namespace:
  ns2 <- "another"
  k2 <- "ccc"

  dr$set_hash(k2, ns2, h)
  testthat::expect_true(dr$exists_hash(k2, ns2))
  testthat::expect_identical(dr$get_hash(k2, ns2), h)

  testthat::expect_identical(dr$list_keys(ns), character(0))
  testthat::expect_identical(dr$list_keys(ns2), "ccc")
  testthat::expect_identical(dr$list_hashes(), h)

  testthat::expect_true(dr$del_hash(k2, ns2))
  testthat::expect_false(dr$exists_hash(k2, ns2))
})

testthat::test_that("traits: throw_missing", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  if (isTRUE(dr$traits[["throw_missing"]])) {
    str <- paste(sample(letters), collapse="")
    testthat::expect_error(dr$get_hash(str, "objects"))
    testthat::expect_error(dr$get_object(hash_object(str)))
  }
})
