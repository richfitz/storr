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

  st <- storr(dr)
  testthat::expect_is(st, "storr")

  ## At this point no namespaces (this might be relaxed)
  testthat::expect_identical(st$list_namespaces(), character(0))

  testthat::expect_identical(st$list(), character(0))
  testthat::expect_identical(st$list_hashes(), character(0))
  ## The objects namespace is allowed here because it's the storr
  ## default namespace; simply querying it above may allow it to come
  ## into being.
  testthat::expect_identical(setdiff(st$list_namespaces(), "objects"),
                             character(0))

  key <- "aaa"

  testthat::expect_error(st$get(key),
                         sprintf("key '%s' ('objects') not found", key),
                         fixed = TRUE,
                         class = "KeyError")

  d <- runif(100)
  hash <- st$hash_object(d)

  res <- st$set(key, d)

  testthat::expect_identical(res, hash)
  testthat::expect_identical(st$list(), key)
  testthat::expect_identical(st$list_hashes(), hash)
  testthat::expect_identical(st$get_hash(key), hash)
  testthat::expect_equal(st$get(key), d, tolerance = 1e-15)
  testthat::expect_equal(st$get(key, use_cache = FALSE), d,
                         tolerance = 1e-15)
  testthat::expect_equal(st$get_value(hash), d, tolerance = 1e-15)
  testthat::expect_identical(ls(st$envir), hash)
  testthat::expect_identical(st$list_namespaces(), "objects")

  ## Set a second key to to the same value:
  key2 <- "bbb"
  st$set(key2, d)
  testthat::expect_identical(sort(st$list()), c(key, key2))
  testthat::expect_identical(st$list_hashes(), hash)
  testthat::expect_identical(st$get_hash(key2), hash)
  testthat::expect_equal(st$get(key2), d, tolerance = 1e-15)
  testthat::expect_equal(st$get(key2, use_cache = FALSE), d,
                         tolerance = 1e-15)

  ## Drop key:
  testthat::expect_true(st$del(key))
  testthat::expect_identical(st$list(), key2)
  testthat::expect_identical(st$list_hashes(), hash)
  testthat::expect_equal(st$get(key2), d, tolerance = 1e-15)

  ## Drop the other key:
  testthat::expect_true(st$del(key2))
  testthat::expect_identical(st$list(), character(0))
  testthat::expect_identical(st$list_hashes(), hash)

  drop <- st$gc()
  testthat::expect_identical(drop, hash)
  testthat::expect_identical(st$list_hashes(), character(0))
  testthat::expect_identical(ls(st$envir), character(0))

  ## Skip the cache on the way in:
  st$set(key2, d, use_cache = FALSE)
  testthat::expect_identical(ls(st$envir), character(0))
  testthat::expect_equal(st$get(key2), d, tolerance = 1e-15)
  testthat::expect_equal(st$get(key2, use_cache = FALSE), d,
                         tolerance = 1e-15)
})


testthat::test_that("replace value", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  st <- storr(dr)

  x <- runif(5)
  y <- runif(10)

  st$set("key", x)
  testthat::expect_equal(st$get("key", use_cache = FALSE), x)

  st$set("key", y)
  testthat::expect_equal(st$get("key", use_cache = FALSE), y)
})


testthat::test_that("default namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  st0 <- storr(dr)
  st <- storr(dr, default_namespace = "storr")
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
  testthat::expect_identical(h, st$hash_object(x))
  testthat::expect_identical(st$list_hashes(), h)
  testthat::expect_identical(st$list(), h)
  testthat::expect_equal(st$get(h), x)
})


testthat::test_that("clear", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)
  st$set("a1", 1, namespace = "a")
  st$set("a2", 2, namespace = "a")
  st$set("b1", 1, namespace = "b")

  testthat::expect_equal(sort(st$list("a")), sort(c("a1", "a2")))
  testthat::expect_equal(st$clear("a"), 2L)
  testthat::expect_equal(st$list("a"), character(0))

  testthat::expect_equal(st$list("b"), "b1")
  testthat::expect_equal(st$clear(NULL), 1L)
  testthat::expect_equal(st$list("b"), character(0))

  st$set("a1", 1, namespace = "a")
  st$set("a2", 2, namespace = "a")
  st$set("b1", 1, namespace = "b")
  testthat::expect_equal(st$clear(NULL), 3L)
  testthat::expect_equal(st$clear(NULL), 0L)
  testthat::expect_equal(st$clear("no_such_namespace"), 0L)
})


test_that("reconnect", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)
  st$set("a1", 1, namespace = "a")
  st$set("a2", 2, namespace = "a")
  st$set("b1", 1, namespace = "b")

  dr2 <- .driver_create(dr)
  st2 <- storr(dr2)

  testthat::expect_equal(st2$list_namespaces(), st$list_namespaces())
  testthat::expect_equal(st2$list("a"), st$list("a"))
  testthat::expect_equal(st2$get("a1", "a"), st$get("a1", "a"))
})


testthat::test_that("hash_algorithm", {
  hash_algos <- c("md5", "sha1")
  x <- runif(10)
  key <- "foo"

  dr <- .driver_create()
  traits <- storr_traits(dr$traits)
  dr$destroy()
  if (!traits$hash_algorithm) {
    skip("hash_algorithm not supported")
  }

  ## TODO: This is very ugly and mimics the behaviour in storr.
  ## However, it's required so that we don't rely on md5 being the
  ## default hash algorithm!
  hmd5 <-
    make_hash_serialized_object("md5", !traits$drop_r_version)(
      make_serialize_object(traits$drop_r_version, traits$accept == "string")(
        x))

  for (h in hash_algos) {
    dr <- .driver_create(hash_algorithm = h)
    on.exit(dr$destroy())

    testthat::expect_equal(dr$hash_algorithm, h)

    st <- storr(dr)
    st$set(key, x)

    testthat::expect_equal(st$get(key), x)
    hash <- st$hash_object(x)
    testthat::expect_equal(st$list_hashes(), hash)
    ## Sanity check
    testthat::expect_equal(hash == hmd5, h == "md5")

    h_other <- setdiff(hash_algos, h)[[1L]]

    ## TODO: doing this with
    ##
    ##   testthat::expect_error(.driver_create(dr, hash_algorithm = h_other),
    ##                           "Incompatible value for hash_algorithm")
    ##
    ## would be preferable, but testthat gives a warning about this up
    ## to version 3.0.0 and thor does not include an appropriate error
    ## class.
    e <- expect_error(.driver_create(dr, hash_algorithm = h_other))
    expect_match(e$message, "Incompatible value for hash_algorithm")

    testthat::expect_equal(.driver_create(dr)$hash_algorithm, h)

    dr$destroy()
  }
  on.exit()
})


testthat::test_that("get_value", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  x <- runif(10)
  st$set("a", x)

  h <- st$list_hashes()
  testthat::expect_equal(st$get_value(h), x)

  testthat::expect_error(st$get_value("nosuchhash"),
                         "hash 'nosuchhash' not found",
                         class = "HashError")
})


## Really simple test to make sure that mget works correctly.  This is
## primarily up to storr, rather than the driver, because we'll test
## mget at the driver level separately.
testthat::test_that("mget", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  h1 <- st$set("foo", 1)
  h2 <- st$set("bar", 2)
  h3 <- st$set("baz", 3, "other")

  testthat::expect_equal(st$mget(character(0)), list())
  testthat::expect_equal(st$mget("foo"), list(1))
  testthat::expect_equal(st$mget(c("foo", "bar")), list(1, 2))
  testthat::expect_equal(st$mget(c("foo", "baz", "bar")),
                         structure(list(1, NULL, 2), missing = 2))

  testthat::expect_equal(st$mget_hash(c("foo", "bar")), c(h1, h2))
  testthat::expect_equal(st$mget_hash(character(0)), character(0))
  testthat::expect_equal(st$mget_hash("baz"), NA_character_)
  testthat::expect_equal(st$mget_hash(c("foo", "baz", "bar")),
                         c(h1, NA, h2))

  testthat::expect_equal(st$mget_hash(c("foo", "baz", "bar"),
                                      c("objects", "other", "objects")),
                         c(h1, h3, h2))
})


testthat::test_that("mset", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  h <- st$mset(c("foo", "bar"), c(1, 2))
  testthat::expect_equal(st$mget_hash(c("foo", "bar")), h)

  ## Multiple namespaces at once:
  h <- st$mset(c("a", "b", "c"), 1:3, c("x", "y", "z"))
  testthat::expect_equal(st$get("a", "x"), 1)
  testthat::expect_equal(st$get("b", "y"), 2)
  testthat::expect_equal(st$get("c", "z"), 3)

  ## TODO: test that when value is the wrong length for the hashes we
  ## throw an error.  The drivers are allowed to assume this.
})


## This is really a test of storr, and if the tests above pass these
## should all pass easily.  Putting them here means that they test
## both the with-mget and without-mget branches though.
testthat::test_that("avoiding caching", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  st$mset(c("a", "b"), 1:2, use_cache = FALSE)
  testthat::expect_equal(ls(st$envir), character(0))
  testthat::expect_equal(st$mget(c("a", "b")), list(1, 2))
})


testthat::test_that("gc", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  x <- runif(10)
  y <- letters
  z <- cos

  hx <- st$set("a", x)
  hy <- st$set("b", y)
  hz <- st$set("c", z)
  st$set("x", x)
  st$set("y", y, "other")

  testthat::expect_equal(st$gc(), character(0))

  st$del("b")
  testthat::expect_equal(st$gc(), character(0))
  st$del("y", "other")
  testthat::expect_equal(st$gc(), hy)

  st$del(c("a", "c", "x"))
  testthat::expect_equal(st$list(), character(0))
  testthat::expect_equal(sort(st$gc()), sort(c(hx, hz)))
})
