test_that("environment", {
  storr::test_driver(function(dr = NULL, ...)
    driver_environment(dr$envir, ...))
})

test_that("rds", {
  storr::test_driver(function(dr = NULL, ...)
    driver_rds(dr$path %||% tempfile("storr_"), ...))
})

test_that("dbi (sqlite)", {
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    new_sqlite <- function() {
      DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    }
    storr::test_driver(function(dr = NULL, ...)
      driver_dbi("data", "keys", dr$con %||% new_sqlite(), ...))
  }
})

test_that("dbi (postgres via RPostgres)", {
  skip_on_cran()
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    if (has_postgres()) {
      pg_create <- function(dr = NULL, ...) {
        if (is.null(dr)) {
          prefix <- paste(sample(letters, 8), collapse = "")
          dr <- list(con = DBI::dbConnect(RPostgres::Postgres()),
                     tbl_data = sprintf("storr_%s_data", prefix),
                     tbl_keys = sprintf("storr_%s_keys", prefix))
        }
        driver_dbi(dr$tbl_data, dr$tbl_keys, dr$con, ...)
      }
      storr::test_driver(pg_create)
    }
  }
})

test_that("multistorr (env + rds)", {
  .driver_create <-
    storr::test_driver(function(dr = NULL, ...)
      driver_multistorr(
        driver_environment(dr$keys$envir),
        driver_rds(dr$data$path %||% tempfile("storr_"), ...)))
})
