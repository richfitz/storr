context("DBI")

## Default will find binary support in SQLite:
test_that("binary detection", {
  skip_if_not_installed("RSQLite")

  dr <- driver_dbi("data", "keys", RSQLite::SQLite(), ":memory:")
  expect_true(dr$binary)

  expect_true(dbi_use_binary(dr$con, "data", NULL))
  expect_true(dbi_use_binary(dr$con, "data", TRUE))
  expect_error(dbi_use_binary(dr$con, "data", FALSE),
               "storage conflicts")

  expect_true(driver_dbi("data", "keys", dr$con)$binary)
  expect_true(driver_dbi("data", "keys", dr$con, binary = TRUE)$binary)
  expect_error(driver_dbi("data", "keys", dr$con, binary = FALSE),
               "storage conflicts")

  dr_s <- driver_dbi("data_s", "key_s", RSQLite::SQLite(), ":memory:", FALSE)
  expect_false(dr_s$binary)

  expect_false(dbi_use_binary(dr_s$con, "data_s", NULL))
  expect_false(dbi_use_binary(dr_s$con, "data_s", FALSE))
  expect_error(dbi_use_binary(dr_s$con, "data_s", TRUE), "storage conflicts")

  expect_false(driver_dbi("data_s", "keys_s", dr_s$con)$binary)
  expect_false(driver_dbi("data_s", "keys_s", dr_s$con, binary = FALSE)$binary)
  expect_error(driver_dbi("data_s", "keys_s", dr_s$con, binary = TRUE),
               "storage conflicts")
})

test_that("connect, reconnect", {
  skip_if_not_installed("RSQLite")
  path <- tempfile()
  dr <- driver_dbi("data", "keys", RSQLite::SQLite(), path)
  st <- storr(dr)

  st$set("foo", "bar")
  expect_equal(st$get("foo"), "bar")

  dr$disconnect()
  expect_null(dr$con)
  expect_error(st$get("foo"))

  dr$reconnect()
  expect_equal(st$get("foo"), "bar")

  dr2 <- driver_dbi("data", "keys", dr$con, NULL)
  st2 <- storr(dr2)
  expect_equal(st2$get("foo"), "bar")
  expect_error(dr2$reconnect(), "Cannot reconnect to this database")

  expect_error(driver_dbi("data", "keys", dr$con, ":memory:"),
               "Cannot specify arguments when passing a connection object")
})

test_that("missing data column", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  st <- storr_dbi("data", "keys", con)

  tbl_data <- "data"
  data_type <- "string"
  sql <- c(sprintf("CREATE TABLE %s", tbl_data),
           "(hash STRING PRIMARY KEY NOT NULL,",
           sprintf("data %s NOT NULL)", data_type))
  DBI::dbGetQuery(con, sprintf("DROP TABLE %s", tbl_data))
  DBI::dbGetQuery(con, paste(sql, collapse = " "))

  expect_error(storr_dbi("data", "keys", con),
               "Did not find 'value' column", fixed = TRUE)
})

test_that("storr", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  st <- storr_dbi("data", "keys", con)
  expect_is(st, "storr")
  expect_equal(st$driver$type(), "DBI/SQLiteConnection")
})

test_that("binary support detection", {
  skip_if_not_installed("mockr")
  ## These can be run without any package support:
  expect_false(dbi_supports_binary(NULL))
  expect_true(dbi_supports_binary(structure(TRUE, class = "SQLiteConnection")))

  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  ## These are pretty easy:
  mockr::with_mock(dbi_supports_binary = function(...) TRUE, {
    expect_true(dbi_use_binary(con, "data", NULL))
    expect_true(dbi_use_binary(con, "data", TRUE))
    expect_false(dbi_use_binary(con, "data", FALSE))
  })

  ## Behaviour when the driver does not support binary:
  ##
  ## TODO: this does not get picked up by covr!?
  mockr::with_mock(dbi_supports_binary = function(...) FALSE, {
    expect_error(dbi_use_binary(con, "data", TRUE),
                 "Binary storage requested but storage driver does")
    expect_false(dbi_use_binary(con, "data", NULL))
    expect_false(dbi_use_binary(con, "data", FALSE))
  })
})

test_that("non-binary storage", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  st <- storr_dbi("data", "keys", con, binary = FALSE)
  x <- runif(10)
  h <- st$hash_object(x)

  st$set("foo", x)
  expect_equal(st$list_hashes(), h)

  expect_equal(st$get_value(h, FALSE), x)

  st$mset(c("a", "b"), list(1, 2))
  expect_equal(st$mget(c("a", "b")), list(1, 2))
})

test_that("unknown dialects", {
  expect_error(driver_dbi_dialect(NULL),
               "Unsupported SQL driver")
})

test_that("old postgres", {
  skip_if_not_installed("mockr")
  con <- structure(list(), class = "PqConnection")
  mockr::with_mock(
    pg_server_version = function(con) numeric_version("0.9.4"),
    expect_error(driver_dbi_sql_compat(con, "a", "b"),
                 "Version 0.9.4 of postgresql server is not supported"))
  mockr::with_mock(
    pg_server_version = function(con) numeric_version("0.9.5"),
    expect_is(driver_dbi_sql_compat(con, "a", "b"), "list"))
})
