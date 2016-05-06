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
  dr <- driver_rds(path, mangle_key=TRUE)
  on.exit(dr$destroy())
  st <- storr(dr)

  st$list()
  st$set("foo", 1)

  expect_identical(st$list(), "foo")


  expect_identical(dir(file.path(path, "keys", "objects")),
                   encode64("foo"))

  st3 <- storr_environment()
  x <- st3$import(st)
  expect_identical(x, "foo")
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

  dr1 <- driver_rds(path, mangle_key=TRUE)
  expect_true(file.exists(file.path(path, "config", "mangle_key")))
  expect_equal(readLines(file.path(path, "config", "mangle_key")), "TRUE")
  expect_true(dr1$mangle_key)

  ## Pointing another driver here without mangling is an error:
  expect_error(driver_rds(path, mangle_key=FALSE),
               "Incompatible mangledness")

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
  expect_error(driver_rds(path2, mangle_key=TRUE),
               "Incompatible mangledness")

  ## But omitting the argument (NULL mangledness) is OK
  dr4 <- driver_rds(path2)
  expect_false(dr4$mangle_key)
})

## This test takes a _lot_ of time and memory.
test_that("large vector support", {
  skip_on_cran()
  skip("this one is really slow but run occasionally")

  data <- raw(2195148826)
  x <- serialize(data, NULL)

  path <- tempfile()
  expect_error(writeBin(x, path), "long vectors not supported yet")
  file.remove(path)

  path <- tempfile()
  dr <- driver_rds(path, compress=FALSE)
  on.exit(dr$destroy())

  ## This is quite slow...
  hash <- hash_object(x, serialize=FALSE, skip=14L)
  ## This is really slow and writes out the data without crashing
  ## ideally.  It does make the system very laggy though and I don't
  ## really understand why.
  dr$set_object(hash, x)

  cmp <- dr$get_object(hash)
  expect_identical(cmp, data)
})
