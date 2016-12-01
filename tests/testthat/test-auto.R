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
      driver_dbi(dr$con %||% new_sqlite(), "data", "keys", ...))
  }
})

test_that("dbi (postgres)", {
  storr::test_driver(
    function(dr = NULL, ...) {
      if (is.null(dr)) {
        prefix <- paste(sample(letters, 8), collapse = "")
        dr <- list(con = DBI::dbConnect(RPostgres::Postgres()),
                   tbl_data = sprintf("storr_%s_data", prefix),
                   tbl_keys = sprintf("storr_%s_keys", prefix))
      }
      driver_dbi(dr$con, dr$tbl_data, dr$tbl_keys, ...)
    }
  )
})

## These are not required on CRAN testing, but only for my own
## edification.
if ("redux" %in% .packages(TRUE)) {
  con <- redux::hiredis()
  storr::test_driver(function(dr = NULL, ...)
    driver_redis_api(dr$prefix %||% rand_str(), con, ...))
}

if ("rrlite" %in% .packages(TRUE)) {
  con <- rrlite::hirlite()
  storr::test_driver(function(dr = NULL, ...)
    driver_redis_api(dr$prefix %||% rand_str(), con, ...))
}
