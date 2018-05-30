context("remote")

test_that("storr spec", {
  create <- function(dr = NULL, ...) {
    ops <- if (is.null(dr)) fake_file_ops(tempfile()) else dr$ops
    driver_remote(ops, ...)
  }

  res <- storr::test_driver(create)
  expect_equal(sum(res$failed), 0)
})


test_that("local cache", {
  local <- tempfile()
  dr <- driver_remote(fake_file_ops(tempfile()), path_local = local)

  on.exit({
    dr$destroy()
    unlink(local, recursive = TRUE)
  })

  expect_true(file.exists(local))
  expect_equal(normalizePath(local), normalizePath(dr$rds$path))

  st <- storr(dr)
  value <- 1:10

  h <- st$set("a", value)
  expect_equal(dr$rds$list_hashes(), h)
  dr$rds$del_object(h)

  expect_equal(st$get("a", use_cache = FALSE), value)
  expect_equal(dr$rds$list_hashes(), h)

  expect_equal(dr$rds$list_keys("objects"), character())
  expect_equal(dr$list_keys("objects"), "a")
})


test_that("list keys works in both manglings", {
  p1 <- tempfile()
  d1 <- driver_remote(fake_file_ops(tempfile()), mangle_key = FALSE)
  s1 <- storr(d1)

  p2 <- tempfile()
  d2 <- driver_remote(fake_file_ops(tempfile()), mangle_key = TRUE)
  s2 <- storr(d2)

  on.exit({
    s1$destroy()
    s2$destroy()
  })

  k <- "silly key name"
  s1$set(k, 1)
  s2$set(k, 1)

  expect_equal(s1$list(), k)
  expect_equal(s2$list(), k)
})
