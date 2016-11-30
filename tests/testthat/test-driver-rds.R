context("driver rds details")

## Tests of the implementation details of the rds driver only...
test_that("creation", {
  path <- tempfile()
  expect_false(file.exists(path))
  dr <- driver_rds(path)
  on.exit(dr$destroy())

  expect_true(file.exists(path))
  expect_identical(sort(dir(path)), c("config", "data", "keys"))
  expect_identical(dir(file.path(path, "data")), character(0))
  expect_false(dr$mangle_key)
})

test_that("mangling", {
  path <- tempfile()
  dr <- driver_rds(path, mangle_key = TRUE)
  on.exit(dr$destroy())
  st <- storr(dr)

  st$list()
  st$set("foo", 1)

  expect_identical(st$list(), "foo")

  expect_identical(dir(file.path(path, "keys", "objects")),
                   encode64("foo"))

  st3 <- storr_environment()
  x <- st3$import(st)
  expect_identical(unname(x[, "name"]), "foo")
  expect_identical(st3$list(), "foo")

  st4 <- storr_environment()
  st4$set("bar", 2)
  st$import(st4)

  expect_identical(sort(st$list()), c("bar", "foo"))
  expect_identical(sort(dir(file.path(path, "keys", "objects"))),
                   sort(c(encode64("foo"), encode64("bar"))))
})

test_that("mangledless compatibility", {
  path <- tempfile()

  dr1 <- driver_rds(path, mangle_key = TRUE)
  expect_true(file.exists(file.path(path, "config", "mangle_key")))
  expect_equal(readLines(file.path(path, "config", "mangle_key")), "TRUE")
  expect_true(dr1$mangle_key)

  ## Pointing another driver here without mangling is an error:
  expect_error(driver_rds(path, mangle_key = FALSE),
               "Incompatible value for mangle_key")

  ## But omitting the argument (NULL mangledness) is OK
  dr2 <- driver_rds(path)
  expect_true(dr2$mangle_key)

  ## In reverse:
  path2 <- tempfile()

  dr3 <- driver_rds(path2)
  expect_true(file.exists(file.path(path2, "config", "mangle_key")))
  expect_equal(readLines(file.path(path2, "config", "mangle_key")), "FALSE")
  expect_false(dr3$mangle_key)

  ## Pointing another driver here without mangling is an error:
  expect_error(driver_rds(path2, mangle_key = TRUE),
               "Incompatible value for mangle_key")

  ## But omitting the argument (NULL mangledness) is OK
  dr4 <- driver_rds(path2)
  expect_false(dr4$mangle_key)
})

## This test takes a lot of time - about 25s.  This really suggests
## that storing objects of this size is not a sensible idea!
test_that("large vector support", {
  skip_on_cran()
  skip_long_test()

  path <- tempfile()
  dr <- driver_rds(path, compress = FALSE)
  on.exit(dr$destroy())
  helper <- spec_helper(dr)

  data <- raw(2195148826)       # ~  1.3s to allocate the data

  x <- helper$serialize(data)   # ~  4.5s
  hash <- helper$hash(x)        # ~  7.0s

  dr$set_object(hash, x)        # ~  8.0s
  cmp <- dr$get_object(hash)    # ~  3.4s
  expect_identical(cmp, data)   # ~  0.3s
  ##                            # -------
  ##                            # ~ 24.5s

  ## Check that R still doesn't support this directly; if it does
  ## we'll move straight over and use the native support (once native
  ## support is present, then the set_object phase will save about 4s
  ## total)
  tmp <- tempfile()
  expect_error(writeBin(x, tmp), "long vectors not supported yet")
  file.remove(tmp)
})

test_that("compression support", {
  ## some data that will likely compress very well:
  data <- rep(1:10, each = 500)

  st1 <- storr_rds(tempfile(), TRUE)
  st2 <- storr_rds(tempfile(), FALSE)
  on.exit({
    st1$destroy()
    st2$destroy()
  })

  h1 <- st1$set("data", data)
  h2 <- st2$set("data", data)

  expect_identical(h1, h2)
  expect_gt(file.size(st2$driver$name_hash(h2)),
            file.size(st1$driver$name_hash(h1)))

  expect_identical(st1$get("data"), data)
  expect_identical(st2$get("data"), data)
})

test_that("backward compatibility", {
  ## In version 1.0.1 and earlier, the hash algorithm was not stored
  ## in the database and md5 was *always* used.
  ##
  ## Was created with:
  ##
  ##   unlink("tests/testthat/v1.0.1", recursive = TRUE)
  ##   storr::storr_rds("tests/testthat/v1.0.1")$set("key", "v1.0.1")
  copy_to_tmp <- function(src) {
    path <- tempfile()
    dir.create(path)
    file.copy(src, path, recursive = TRUE)
    file.path(path, src)
  }

  path <- copy_to_tmp("v1.0.1")
  st <- storr_rds(path)
  expect_equal(st$list(), "key")
  expect_equal(st$get("key"), "v1.0.1")
  expect_equal(st$driver$hash_algorithm, "md5")
  st$destroy()

  path <- copy_to_tmp("v1.0.1")
  expect_error(storr_rds(path, hash_algorithm = "sha1"),
               "Incompatible value for hash_algorithm")
})
