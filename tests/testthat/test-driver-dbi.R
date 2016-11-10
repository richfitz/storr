context("DBI")

## Default will find binary support in SQLite:
test_that("binary detection", {
  ## TODO: for CRAN safety this needs to be skipped if SQLite is not
  ## of sufficient version.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  db <- driver_dbi(con, "data", "keys")
  expect_true(db$binary)
  expect_true(driver_dbi(con, "data", "keys")$binary)
  expect_error(driver_dbi(con, "data", "keys", FALSE),
               "storage conflicts")

  db_s <- driver_dbi(con, "data_s", "key_s", FALSE)
  expect_false(db_s$binary)
  expect_false(driver_dbi(con, "data_s", "keys_s")$binary)
  expect_error(driver_dbi(con, "data_s", "keys_s", TRUE),
               "storage conflicts")
})

test_that("hash_algorithm", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  hash_algos <- c("md5", "sha1")
  x <- runif(10)
  key <- "foo"
  hmd5 <- digest::digest(x, "md5")
  h <- "md5"

  for (h in hash_algos) {
    st <- storr_dbi(con, "data", "keys", hash_algorithm = h)
    st$set(key, x)
    expect_equal(st$get(key), x)
    hash <- digest::digest(x, h)
    expect_true(st$exists_object(hash))
    ## Sanity check
    expect_equal(hash == hmd5, h == "md5")

    h_other <- setdiff(hash_algos, h)[[1]]
    expect_error(storr_dbi(con, "data", "keys", hash_algorithm = h_other),
                 "Incompatible value for hash_algorithm")

    expect_equal(storr_dbi(con, "data", "keys")$driver$hash_algorithm, h)

    st$destroy()
  }
})
