context("DBI")

## Default will find binary support in SQLite:
test_that("binary detection", {
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
