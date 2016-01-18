## This requires
##   .driver_name: character(1)
##   .driver_create: function()
testthat::context(sprintf("storr [%s]", .driver_name))

## With the refactor, the aim here is to change storr files only by
## *addition*; that means that we've kept a compatible framework.
## Deletions and modifications are to be avoided, though they might
## get done at a later point.

## I know that klmr is doing similar testing framework things with
## DBI; might be worth seeing what he's up to and if there's any
## shared infrastructure there.
testthat::test_that("basic", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  cache <- storr(dr)
  testthat::expect_is(cache, "storr")

  ## At this point no namespaces (this might be relaxed)
  testthat::expect_identical(cache$list_namespaces(), character(0))

  testthat::expect_identical(cache$list(), character(0))
  testthat::expect_identical(cache$list_hashes(), character(0))
  ## The objects namespace is allowed here because it's the storr
  ## default namespace; simply querying it above may allow it to come
  ## into being.
  testthat::expect_identical(setdiff(cache$list_namespaces(), "objects"),
                             character(0))

  testthat::expect_error(cache$get("aaa"),
                         "key 'aaa' ('objects') not found", fixed=TRUE)

  d <- runif(100)
  hash <- hash_object(d)

  res <- cache$set("aaa", d)

  testthat::expect_identical(res, hash)
  testthat::expect_identical(cache$list(), "aaa")
  testthat::expect_identical(cache$list_hashes(), hash)
  testthat::expect_identical(cache$get_hash("aaa"), hash)
  testthat::expect_equal(cache$get("aaa"), d, tolerance=1e-15)
  testthat::expect_equal(cache$get("aaa", use_cache=FALSE), d, tolerance=1e-15)
  testthat::expect_equal(cache$get_value(hash), d, tolerance=1e-15)
  testthat::expect_identical(ls(cache$envir), hash)
  testthat::expect_identical(cache$list_namespaces(), "objects")

  ## Set a second key to to the same value:
  cache$set("bbb", d)
  testthat::expect_identical(sort(cache$list()), c("aaa", "bbb"))
  testthat::expect_identical(cache$list_hashes(), hash)
  testthat::expect_identical(cache$get_hash("bbb"), hash)
  testthat::expect_equal(cache$get("bbb"), d, tolerance=1e-15)
  testthat::expect_equal(cache$get("bbb", use_cache=FALSE), d, tolerance=1e-15)

  ## Drop key:
  testthat::expect_true(cache$del("aaa"))
  testthat::expect_identical(cache$list(), "bbb")
  testthat::expect_identical(cache$list_hashes(), hash)
  testthat::expect_equal(cache$get("bbb"), d, tolerance=1e-15)

  ## Drop the other key:
  testthat::expect_true(cache$del("bbb"))
  testthat::expect_identical(cache$list(), character(0))
  testthat::expect_identical(cache$list_hashes(), hash)

  drop <- cache$gc()
  testthat::expect_identical(drop, hash)
  testthat::expect_identical(cache$list_hashes(), character(0))
  testthat::expect_identical(ls(cache$envir), character(0))

  ## Skip the cache on the way in:
  cache$set("bbb", d, use_cache=FALSE)
  testthat::expect_identical(ls(cache$envir), character(0))
  testthat::expect_equal(cache$get("bbb"), d, tolerance=1e-15)
  testthat::expect_equal(cache$get("bbb", use_cache=FALSE), d, tolerance=1e-15)
})

testthat::test_that("default namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  st0 <- storr(dr)
  st <- storr(dr, default_namespace="storr")
  st1 <- storr(dr)

  testthat::expect_identical(st$default_namespace, "storr")

  st$set("foo", 1:10)
  testthat::expect_identical(st$list("objects"), character(0))
  testthat::expect_identical(st$list("storr"), "foo")

  st0$set("foo", letters)
  testthat::expect_identical(st0$get("foo"), letters)
  testthat::expect_identical(st$get("foo"), 1:10)
})

## Little helper:
testthat::test_that("set_by_value", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)
  x <- runif(10)
  h <- st$set_by_value(x)
  testthat::expect_identical(h, hash_object(x))
  testthat::expect_identical(st$list_hashes(), h)
  testthat::expect_identical(st$list(), h)
  testthat::expect_equal(st$get(h), x)
})

testthat::test_that("clear", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)
  st$set("a1", 1, namespace="a")
  st$set("a2", 2, namespace="a")
  st$set("b1", 1, namespace="b")

  testthat::expect_equal(sort(st$list("a")), sort(c("a1", "a2")))
  testthat::expect_equal(st$clear("a"), 2L)
  testthat::expect_equal(st$list("a"), character(0))

  testthat::expect_equal(st$list("b"), "b1")
  testthat::expect_equal(st$clear(NULL), 1L)
  testthat::expect_equal(st$list("b"), character(0))

  st$set("a1", 1, namespace="a")
  st$set("a2", 2, namespace="a")
  st$set("b1", 1, namespace="b")
  testthat::expect_equal(st$clear(NULL), 3L)
  testthat::expect_equal(st$clear(NULL), 0L)
  testthat::expect_equal(st$clear("no_such_namespace"), 0L)
})
