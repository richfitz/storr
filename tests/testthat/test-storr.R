context("storr")

test_that("flush cache", {
  st <- storr_environment()
  expect_equal(ls(st$envir), character(0))

  x <- runif(10)
  st$set("a", x)

  h <- st$list_hashes()
  expect_equal(ls(st$envir), st$list_hashes())

  ## check that the cache works by poisoning it:
  expect_equal(st$get("a"), x)
  st$envir[[h]] <- letters
  expect_equal(st$get("a"), letters)
  expect_equal(st$get("a", use_cache = FALSE), x)

  st$flush_cache()
  expect_equal(ls(st$envir), character(0))
})

test_that("traits", {
  expect_equal(storr_traits(NULL), storr_traits_default())
  expect_equal(storr_traits(list()), storr_traits_default())
  expect_error(storr_traits(list(infinite_unicorns = TRUE)),
               "Unknown traits")
  expect_error(storr_traits(list(drop_r_version = TRUE, accept = "object")),
               "if 'drop_r_version' is TRUE, then 'accept'",
               fixed = TRUE)
  expect_error(storr_traits(list(drop_r_version = TRUE, accept = "string")),
               "if 'drop_r_version' is TRUE, then 'accept'",
               fixed = TRUE)
  expect_error(storr_traits(list(accept = "foo")),
               "must be one of")
  expect_error(storr_traits(list(accept = NULL)),
               "must be a scalar")
})

test_that("mset edge cases", {
  st <- storr_environment()
  expect_error(st$mset(c("a", "b"), list(1, 2, 3), c("x", "y", "z")),
               "Incompatible lengths for key and namespace")
  expect_error(st$mset(c("a", "b"), list(1, 2, 3)),
               "'value' must have 2 elements")
})

test_that("missing", {
  st <- storr_environment()
  x <- st$mget("foo")
  expect_equal(x, structure(list(NULL), missing = 1L))

  x <- st$mget("foo", missing = NA_integer_)
  expect_equal(x, structure(list(NA_integer_), missing = 1L))

  st$set("a", 1)
  x <- st$mget(c("a", "b", "a"))
  expect_equal(x, structure(list(1, NULL, 1), missing = 2L))
})

test_that("mset_by_value", {
  st <- storr_environment()
  x <- runif(10)
  y <- runif(20)
  h <- st$mset_by_value(list(x, y))
  expect_equal(h, c(st$hash_object(x), st$hash_object(y)))
  expect_equal(st$exists(h), c(TRUE, TRUE))
  expect_equal(st$get(h[[1]]), x)
  expect_equal(st$get(h[[2]]), y)
})

test_that("duplicate", {
  st <- storr_environment()

  h1 <- st$set("a", runif(10))
  expect_null(st$duplicate("a", "b"))

  expect_identical(st$get("b"), st$get("a"))
  expect_identical(st$get_hash("b"), st$get_hash("a"))
})

test_that("fill", {
  st <- storr_environment()
  v <- runif(10)
  h <- st$fill(letters, v)
  expect_equal(h, st$hash_object(v))
  expect_equal(st$mget(letters),
               rep(list(v), length(letters)))
})
