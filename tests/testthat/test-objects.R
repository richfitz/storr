context("objects")

test_that("basic", {
  drivers <- create_drivers()
  on.exit(cleanup_drivers(drivers))

  for (dr in drivers) {
    cache <- object_cache(dr)
    expect_that(cache, is_a("object_cache"))

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
    expect_that(cache$get("aaa", FALSE), equals(d))
    expect_that(cache$get_value(hash), equals(d))
    expect_that(ls(cache$envir), equals(hash))

    ## Set a second key to to the same value:
    cache$set("bbb", d)
    expect_that(sort(cache$list()), equals(c("aaa", "bbb")))
    expect_that(cache$list_hashes(), equals(hash))
    expect_that(cache$get_hash("bbb"), equals(hash))
    expect_that(cache$get("bbb"), equals(d))
    expect_that(cache$get("bbb", FALSE), equals(d))

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
    cache$set("bbb", d, FALSE)
    expect_that(ls(cache$envir), equals(character(0)))
    expect_that(cache$get("bbb"), equals(d))
    expect_that(cache$get("bbb", FALSE), equals(d))
  }
})
