context("driver rds details")

## Tests of the implementation details of the rds driver only...
test_that("creation", {
  path <- tempfile()
  expect_false(file.exists(path))
  dr <- driver_rds(path)
  on.exit(dr$destroy())

  expect_true(file.exists(path))
  expect_identical(sort(dir(path)), c("config", "data", "keys", "scratch"))
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

test_that("mangledness padding compatibility", {
  path <- tempfile()

  dr1 <- driver_rds(path, mangle_key = TRUE)
  expect_true(file.exists(file.path(path, "config", "mangle_key_pad")))
  expect_equal(readLines(file.path(path, "config", "mangle_key_pad")), "FALSE")
  expect_false(dr1$mangle_key_pad)

  ## Pointing another driver here without mangling is an error:
  expect_error(driver_rds(path, mangle_key_pad = TRUE),
               "Incompatible value for mangle_key_pad")

  ## But omitting the argument (NULL mangledness) is OK
  dr2 <- driver_rds(path)
  expect_false(dr2$mangle_key_pad)

  ## In reverse:
  path2 <- tempfile()

  dr3 <- driver_rds(path2, mangle_key = TRUE, mangle_key_pad = TRUE)
  expect_true(file.exists(file.path(path2, "config", "mangle_key_pad")))
  expect_equal(readLines(file.path(path2, "config", "mangle_key_pad")), "TRUE")
  expect_true(dr3$mangle_key_pad)

  ## Pointing another driver here without mangling is an error:
  expect_error(driver_rds(path2, mangle_key = TRUE, mangle_key_pad = FALSE),
               "Incompatible value for mangle_key")

  ## But omitting the argument (NULL mangledness) is OK
  dr4 <- driver_rds(path2)
  expect_true(dr4$mangle_key_pad)
})

## This test takes a lot of time - about 25s.  This really suggests
## that storing objects of this size is not a sensible idea!
test_that("large vector support", {
  skip_on_cran()
  skip_long_test()

  path <- tempfile()
  dr <- driver_rds(path, compress = FALSE)
  on.exit(dr$destroy())
  st <- storr(dr)

  data <- raw(2195148826)       # ~  1.3s to allocate the data

  x <- st$serialize_object(data)# ~  4.5s
  hash <- st$hash_raw(x)        # ~  7.0s

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
  path <- copy_to_tmp("v1.0.1_clear")
  st <- storr_rds(path)
  expect_equal(st$list(), "key")
  expect_equal(st$get("key"), "v1.0.1")
  expect_equal(st$driver$hash_algorithm, "md5")
  expect_false(st$driver$mangle_key)
  st$destroy()

  path <- copy_to_tmp("v1.0.1_clear")
  expect_error(storr_rds(path, hash_algorithm = "sha1"),
               "Incompatible value for hash_algorithm")
})

test_that("mangledness padding backward compatibility", {
  ## In version 1.0.1 and earlier, mangling was always padded
  path <- copy_to_tmp("v1.0.1_mangled")
  st <- storr_rds(path)
  expect_true(st$driver$mangle_key)
  expect_true(st$driver$mangle_key_pad)
  expect_equal(st$get("a"), 1)
  expect_equal(st$get("bb"), 2)
  expect_equal(st$get("ccc"), 3)
  expect_equal(st$list(),
               sort(c("a", "bb", "ccc")))
  st$set("a", "x")
  st$set("bb", "y")
  st$set("ccc", "z")
  expect_equal(st$list(),
               sort(c("a", "bb", "ccc")))
  st$destroy()
})

## This is a test for issue 42; check that hard links do not create
## inconsistent storrs.
test_that("copy -lr and consistency", {
  skip_on_cran()
  skip_on_os(c("windows", "mac", "solaris"))

  path1 <- tempfile()
  path2 <- tempfile()
  st1 <- storr_rds(path1)
  h1 <- st1$set("foo", "val1")

  ok <- system2("cp", c("-lr", path1, path2))

  st2 <- storr_rds(path2)
  expect_equal(st2$get("foo"), "val1")

  h2 <- st1$set("foo", "val2")
  expect_equal(st1$get("foo"), "val2")
  expect_equal(st2$get("foo"), "val1")

  expect_equal(st2$list_hashes(), h1)
  expect_equal(sort(st1$list_hashes()), sort(c(h1, h2)))
})

## Prevent a regression
test_that("vectorised exists", {
  st <- storr_rds(tempfile(), mangle_key = TRUE)
  on.exit(st$destroy())

  expect_equal(st$exists(c("x", "xx")), c(FALSE, FALSE))
  st$set("x", 1)
  st$set("xx", 2)
  expect_equal(st$exists(c("x", "xx")), c(TRUE, TRUE))
})

test_that("change directories and access same storr", {
  x <- storr_rds("my_storr")
  x$set("a", 1)
  expect_equal(x$list(), "a")
  subdir <- "subdir"
  dir.create(subdir)
  owd <- setwd(subdir)
  on.exit(setwd(owd))
  expect_equal(x$list(), "a")
  setwd("..")
  unlink(subdir, recursive = TRUE)
  x$destroy()
})

test_that("check empty storr", {
  st <- storr_rds(tempfile())
  expect_true(st$check()$healthy)
  expect_silent(st$check(quiet = TRUE))
})

test_that("recover corrupt storr: corrupted rds", {
  st <- storr_rds(tempfile())

  ## First start with a storr with some data in it:
  for (i in 1:10) {
    st$mset(paste0(letters[[i]], seq_len(i)),
            lapply(seq_len(i), function(.) runif(20)),
            namespace = LETTERS[[i]])
  }

  res <- st$check()
  expect_true(res$healthy)

  ## Then let's truncate some data!
  set.seed(1)
  i <- sample.int(55, 5)
  r <- st$list_hashes()[i]
  for (p in st$driver$name_hash(r)) {
    writeBin(raw(), p)
  }

  res <- st$check()
  expect_is(res, "storr_check")
  expect_false(res$healthy)

  expect_equal(length(res$objects$corrupt), 5L)
  expect_equal(nrow(res$keys$corrupt), 0L)
  expect_equal(nrow(res$keys$dangling), 5L)

  res2 <- st$check(full = FALSE)
  expect_false(res2$healthy)
  expect_equal(res2$objects, res$objects)
  expect_equal(nrow(res2$keys$corrupt), 0L)
  expect_equal(nrow(res2$keys$dangling), 0L)

  st$repair(force = TRUE)
  res <- st$check()
  expect_true(res$healthy)
  expect_false(st$repair(res, force = TRUE))
  expect_silent(st$repair(res, force = TRUE))
  expect_equal(st$check(full = FALSE), res)
})


test_that("don't run automatically", {
  skip_if_interactive()
  st <- storr_rds(tempfile())
  for (i in 1:10) {
    st$mset(paste0(letters[[i]], seq_len(i)),
            lapply(seq_len(i), function(.) runif(20)),
            namespace = LETTERS[[i]])
  }
  set.seed(1)
  i <- sample.int(55, 5)
  r <- st$list_hashes()[i]
  for (p in st$driver$name_hash(r)) {
    writeBin(raw(), p)
  }

  expect_error(st$repair(force = FALSE),
               "Please rerun with force")
})


test_that("automatic is ok if storr is healthy", {
  expect_false(storr_rds(tempfile())$repair(force = FALSE))
})


test_that("corrupted mangled keys", {
  st <- storr_rds(tempfile(), mangle_key = TRUE, mangle_key_pad = TRUE)
  st$mset(month.name,
          lapply(seq_along(month.name), function(.) runif(20)))
  keys <- st$driver$name_key(month.name, "objects")
  file.copy(keys[[3]],
            paste(keys[[3]], "(conflicted copy)"))
  with_options(list("storr.corrupt.notice.period" = NA),
               expect_silent(st$list()))
  expect_message(st$list(),
                 "1 corrupted files have been found in your storr archive:")
  expect_silent(st$list())
  with_options(list("storr.corrupt.notice.period" = 0L),
               expect_message(st$list(), "namespace: 'objects'"))

  expect_message(st$driver$purge_corrupt_keys("objects"),
                 "Removed 1 of 1 corrupt file")
  with_options(list("storr.corrupt.notice.period" = 0L),
               expect_silent(st$list()))
  expect_silent(st$driver$purge_corrupt_keys("objects"))
})
