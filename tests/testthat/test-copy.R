context("copy")

test_that("copy list", {
  st <- storr_environment()

  src <- list(a = 1, b = 2)
  res <- st$import(src)
  expect_equal(sort(res), sort(names(src)))

  expect_equal(st$get("a"), src$a)
  expect_equal(st$get("b"), src$b)
})

test_that("copy unsupported", {
  st <- storr_environment()
  expect_error(st$import(runif(10)),
               "Invalid type for src")
})

test_that("as.list", {
  st <- storr_environment()
  src <- list(a = runif(10), b = runif(20))
  res <- st$import(src)
  dat <- as.list(st)
  expect_equal(dat, src[names(dat)])
})
