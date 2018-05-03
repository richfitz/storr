context("hash")

test_that("serialize, dropping R version", {
  s1 <- make_serialize_object(FALSE, FALSE)(NULL) # no drop, binary
  s2 <- make_serialize_object(TRUE,  FALSE)(NULL) #    drop, binary
  expect_false(identical(s1, s2))
  i <- 7:10
  expect_identical(s1[-i], s2[-i])
  expect_identical(s2[i], STORR_R_VERSION_BE)

  expect_identical(unserialize(s1), unserialize(s2))
})

test_that("reverse engineering", {
  skip_on_cran()
  ## NOTE: These tests are skipped on CRAN because they depend on
  ## wording of error messages.  But I still want to see how this
  ## varies across R versions so I run this on travis.

  ## With xdr = TRUE (the default)
  ##
  ##                           ma mi pa    ma mi pa
  ##  [1] 58 0a 00 00 00 02 00 03 02 03 00 02 03 00 -- 3.2.3
  ##  [1] 58 0a 00 00 00 02 00 03 03 00 00 02 03 00 -- 3.3.1
  ##      ^^^^^ ^^^^^^^^^^^ ^^^^^^^^^^^ ^^^^^^^^^^^
  ##      1.    2.          3.          4.
  ##
  ## with xdr = FALSE
  ##
  ##                        pa mi ma    pa mi ma
  ##  [1] 42 0a 02 00 00 00 03 02 03 00 00 03 02 00 -- 3.2.3
  ##  [1] 42 0a 02 00 00 00 00 03 03 00 00 03 02 00 -- 3.3.1
  ##      ^^^^^ ^^^^^^^^^^^ ^^^^^^^^^^^ ^^^^^^^^^^^
  ##      1.   2.           3.          4.

  ## 1. serialisation type
  ## 2. version of the serialisation (must be 2 at present)
  ## 3. version of R written by
  ## 4. minimum version of r to read the file

  s1 <- make_serialize_object(FALSE, FALSE,  TRUE)(NULL) # xdr = TRUE
  s2 <- make_serialize_object(FALSE, FALSE, FALSE)(NULL) # xdr = FALSE

  s1[6] <- as.raw(9L)
  expect_error(unserialize(s1), "cannot read workspace version 9")
  s1[7:10] <- as.raw(c(0L, 6L, 5L, 4L)) # version 6.5.4
  expect_error(unserialize(s1), "6.5.4", fixed = TRUE)

  s2[3] <- as.raw(9L)
  expect_error(unserialize(s2), "cannot read workspace version 9")
  s2[10:7] <- as.raw(c(0L, 6L, 5L, 4L)) # version 6.5.4
  expect_error(unserialize(s2), "6.5.4", fixed = TRUE)
})

test_that("unserialize safe", {
  expect_error(unserialize_safe(NULL), "Invalid input")
  x <- runif(10)
  expect_identical(unserialize_safe(serialize(x, NULL)), x)
  if (getRversion() >= numeric_version("3.2.0")) {
    expect_identical(unserialize_safe(serialize(x, NULL, NA)), x)
  }
})

test_that("make_serialize_object", {
  expect_error(make_serialize_object(FALSE, TRUE,
                                     r_version = numeric_version("3.1.2")),
               "upgrade R")
  expect_error(make_serialize_object(TRUE, TRUE),
               "Can't combine drop_r_version and string serialization")
})
