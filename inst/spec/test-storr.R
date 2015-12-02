## This requires
##   .driver_name: character(1)
##   .driver_create: function()
context(sprintf("storr [%s]", .driver_name))

## I know that klmr is doing similar testing framework things with
## DBI; might be worth seeing what he's up to and if there's any
## shared infrastructure there.
test_that("basic", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  cache <- storr(dr)
  expect_that(cache, is_a("storr"))

  expect_that(cache$list(), equals(character(0)))
  expect_that(cache$list_hashes(), equals(character(0)))

  expect_that(cache$get("aaa"), throws_error("key 'aaa' not found"))

  d <- runif(100)
  hash <- hash_object(d)

  cache$set("aaa", d)

  expect_that(cache$list(), equals("aaa"))
  expect_that(cache$list_hashes(), equals(hash))
  expect_that(cache$get_hash("aaa"), equals(hash))
  expect_that(cache$get("aaa"), equals(d))
  expect_that(cache$get("aaa", use_cache=FALSE), equals(d))
  expect_that(cache$get_value(hash), equals(d))
  expect_that(ls(cache$envir), equals(hash))

  ## Set a second key to to the same value:
  cache$set("bbb", d)
  expect_that(sort(cache$list()), equals(c("aaa", "bbb")))
  expect_that(cache$list_hashes(), equals(hash))
  expect_that(cache$get_hash("bbb"), equals(hash))
  expect_that(cache$get("bbb"), equals(d))
  expect_that(cache$get("bbb", use_cache=FALSE), equals(d))

  ## Drop key:
  expect_that(cache$del("aaa"), is_true())
  expect_that(cache$list(), equals("bbb"))
  expect_that(cache$list_hashes(), equals(hash))
  expect_that(cache$get("bbb"), equals(d))

  ## Drop the other key:
  expect_that(cache$del("bbb"), is_true())
  expect_that(cache$list(), equals(character(0)))
  expect_that(cache$list_hashes(), equals(hash))

  drop <- cache$gc()
  expect_that(drop, equals(hash))
  expect_that(cache$list_hashes(), equals(character(0)))
  expect_that(ls(cache$envir), equals(character(0)))

  ## Skip the cache on the way in:
  cache$set("bbb", d, use_cache=FALSE)
  expect_that(ls(cache$envir), equals(character(0)))
  expect_that(cache$get("bbb"), equals(d))
  expect_that(cache$get("bbb", use_cache=FALSE), equals(d))
})

test_that("default namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  st0 <- storr(dr)
  st <- storr(dr$copy(), default_namespace="storr")
  st1 <- storr(dr)

  expect_that(formals(st0$type)$namespace, equals("objects"))
  expect_that(formals(st$type)$namespace, equals("storr"))
  expect_that(formals(st1$type)$namespace, equals("objects"))

  for (m in ls(st)) {
    ff <- formals(st[[m]])
    if ("namespace" %in% names(ff)) {
      expect_that(ff$namespace, equals("storr"))
    }
  }

  st$set("foo", 1:10)
  expect_that(st$list("objects"), equals(character(0)))
  expect_that(st$list("storr"), equals("foo"))

  st0$set("foo", letters)
  expect_that(st0$get("foo"), equals(letters))
  expect_that(st$get("foo"), equals(1:10))
})

## Little helper:
test_that("set_by_value", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)
  x <- runif(10)
  h <- st$set_by_value(x)
  expect_identical(h, hash_object(x))
  expect_identical(st$list_hashes(), h)
  expect_identical(st$list(), h)
  expect_equal(st$get(h), x)
})
