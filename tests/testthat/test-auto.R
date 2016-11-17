test_that("environment", {
  storr:::test_driver(function(dr = NULL, ...)
    driver_environment(dr$envir, ...))
})

test_that("rds", {
  storr:::test_driver(function(dr = NULL, ...)
    driver_rds(dr$path %||% tempfile("storr_"), ...))
})

test_that("dbi", {
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    new_sqlite <- function() {
      DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    }
    storr:::test_driver(function(dr = NULL, ...)
      driver_dbi(dr$con %||% new_sqlite(), "data", "keys", ...))
  }
})

## These are not required on CRAN testing, but only for my own
## edification.
if ("redux" %in% .packages(TRUE)) {
  rand_str <- function() paste0(hash_object(Sys.time()), ":")
  con <- redux::hiredis()
  storr:::test_driver(function(dr = NULL, ...)
    driver_redis_api(dr$prefix %||% rand_str(), con, ...))
}

if ("rrlite" %in% .packages(TRUE)) {
  rand_str <- function() hash_object(Sys.time())
  con <- rrlite::hirlite()
  storr:::test_driver(function(dr = NULL, ...)
    driver_redis_api(dr$prefix %||% rand_str(), con, ...))
}
