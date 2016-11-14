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

test_that("storr", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  st <- storr_dbi(con, "data", "keys")
  expect_is(st, "storr")
  expect_equal(st$driver$type(), "DBI/SQLiteConnection")
})

test_that("binary support detection", {
  ## Fake package version generating function:
  pv <- function(v) {
    force(v)
    function(pkg, lib.loc = NULL) {
      if (pkg %in% names(v)) {
        ret <- v[[pkg]]
      } else {
        ret <- packageDescription(pkg, lib.loc = lib.loc, fields = "Version")
      }
      numeric_version(ret)
    }
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  ## These are pretty easy:
  with_mock("storr::dbi_supports_binary" = function(...) TRUE, {
    expect_true(dbi_use_binary(con, "data", NULL))
    expect_true(dbi_use_binary(con, "data", TRUE))
    expect_false(dbi_use_binary(con, "data", FALSE))
  })

  ## Behaviour when the driver does not support binary:
  ##
  ## TODO: this does not get picked up by covr!?
  with_mock("storr::dbi_supports_binary" = function(...) FALSE, {
    expect_error(dbi_use_binary(con, "data", TRUE),
                 "Binary storage requested but storage driver does")
    expect_false(dbi_use_binary(con, "data", NULL))
    expect_false(dbi_use_binary(con, "data", FALSE))
  })

  ## Then test version things:
  with_mock("utils::packageVersion" = pv(c(DBI = "0.0.1")),
            expect_false(dbi_supports_binary(con)))
  with_mock("utils::packageVersion" = pv(c(RSQLite = "0.0.1")),
            expect_false(dbi_supports_binary(con)))
  with_mock("utils::packageVersion" = pv(c(RSQLite = "1.0.0", DBI = "0.4.1")),
            expect_true(dbi_supports_binary(con)))
})

test_that("non-binary storage", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  st <- storr_dbi(con, "data", "keys", binary = FALSE)
  x <- runif(10)
  h <- hash_object(x)

  st$set("foo", x)
  expect_equal(st$list_hashes(), h)

  expect_equal(st$get_value(h, FALSE), x)
})
