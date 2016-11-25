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

test_that("unknown types", {
  st <- storr_environment()

  src <- list(a = 1, b = 2)
  expect_error(
    st$import(unlist(src), list = names(src)),
    "Invalid type for src; can't 'get' from objects of type numeric")

  res <- st$import(src)

  expect_error(
    st$export(numeric(), list = names(res)),
    "Invalid type for dest; can't 'set' into objects of type numeric")
})

test_that("copy with different hash algorithms", {
  st_md5 <- storr_environment(hash_algorithm = "md5")
  st_sha <- storr_environment(hash_algorithm = "sha1")

  x <- runif(10)
  h <- st_md5$set("x", x)

  expect_equal(st_sha$import(st_md5), "x")
  expect_equal(st_sha$get("x"), x)
  expect_false(st_sha$get_hash("x") == h)
})
