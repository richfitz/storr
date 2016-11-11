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

test_that("missing data column", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  st <- storr_dbi(con, "data", "keys")

  tbl_data <- "data"
  data_type <- "string"
  sql <- c(sprintf("CREATE TABLE %s", tbl_data),
           "(hash STRING PRIMARY KEY NOT NULL,",
           sprintf("data %s NOT NULL)", data_type))
  DBI::dbGetQuery(con, sprintf("DROP TABLE %s", tbl_data))
  DBI::dbGetQuery(con, paste(sql, collapse = " "))

  expect_error(storr_dbi(con, "data", "keys"),
               "Did not find 'value' column", fixed = TRUE)
})
