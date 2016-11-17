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
  expect_error(storr_traits(list(drop_r_version = TRUE,
                                 accept_raw = FALSE)),
               "if drop_r_version is TRUE, then accept_raw must also be TRUE",
               fixed = TRUE)
})
