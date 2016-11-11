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
