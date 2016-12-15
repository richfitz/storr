## This requires
##   .driver_name: character(1)
##   .driver_create: function()
testthat::context(sprintf("storr_direct [%s]", .driver_name))

testthat::test_that("driver (empty)", {
  skip_if_no_direct(.driver_name, .driver_create)

  dr <- .driver_create(hash_algorithm = NOHASH)
  testthat::expect_true(dr$is_direct)
  testthat::expect_equal(dr$hash_algorithm, NOHASH)

  testthat::expect_equal(dr$type(), .driver_name)

  ns <- "objects"
  testthat::expect_equal(dr$list_namespaces(), character(0))
  testthat::expect_equal(dr$list_keys(ns), character(0))

  k <- "bbb"
  ns <- "ns"
  testthat::expect_false(dr$exists_hash(k, ns))
  testthat::expect_false(dr$del_hash(k, ns))
})

testthat::test_that("driver; core operations", {
  skip_if_no_direct(.driver_name, .driver_create)

  dr <- .driver_create(hash_algorithm = NOHASH)

  k <- "aaa"
  ns <- "ns"
  v <- runif(10)

  dr$set_hash(k, ns, v)
  testthat::expect_equal(dr$get_hash(k, ns), v)
  testthat::expect_true(dr$exists_hash(k, ns))
  testthat::expect_equal(dr$list_keys(ns), k)
  testthat::expect_true(dr$del_hash(k, ns))
  testthat::expect_false(dr$del_hash(k, ns))
  testthat::expect_equal(dr$list_keys(ns), character(0))
})

testthat::test_that("storr", {
  skip_if_no_direct(.driver_name, .driver_create)

  dr <- .driver_create(hash_algorithm = NOHASH)
  testthat::expect_true(dr$is_direct)

  st <- storr(dr)
  testthat::expect_is(st, "storr_direct")
  testthat::expect_null(st$set_by_value)

  testthat::expect_equal(st$list(), character(0))
  ## TODO: I don't know that this is correct...
  ## testthat::expect_equal(st$list_namespaces(), st$default_namespace)

  x <- runif(10)
  k <- rand_str(5)
  st$set(k, x)
  testthat::expect_equal(st$get(k), x)
  testthat::expect_equal(st$list(), k)
  testthat::expect_equal(st$list_namespaces(), st$default_namespace)
  testthat::expect_true(st$exists(k))
  testthat::expect_true(st$del(k))
  testthat::expect_false(st$del(k))
})

testthat::test_that("storr; bulk operations", {
  skip_if_no_direct(.driver_name, .driver_create)

  dr <- .driver_create(hash_algorithm = NOHASH)
  st <- storr(dr)

  k <- replicate(10, rand_str(5))
  x <- runif(length(k))

  testthat::expect_equal(st$mset(k, x), length(k))
  testthat::expect_equal(st$list(), sort(k))
  testthat::expect_equal(st$exists(k), rep(TRUE, length(k)))

  testthat::expect_equal(st$mget(k), as.list(x))
  i <- sample.int(length(k), 5)
  testthat::expect_equal(st$del(k[i]), rep(TRUE, length(i)))
  testthat::expect_equal(st$del(k), !(seq_along(k) %in% i))
})
