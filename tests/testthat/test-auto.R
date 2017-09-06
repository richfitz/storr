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

pg_tester <- function(ctor) {
  force(ctor)
  function(dr = NULL, ...) {
    if (is.null(dr)) {
      prefix <- paste(sample(letters, 8), collapse = "")
      dr <- list(con = DBI::dbConnect(ctor()),
                 tbl_data = sprintf("storr_%s_data", prefix),
                 tbl_keys = sprintf("storr_%s_keys", prefix))
    }
    driver_dbi(dr$tbl_data, dr$tbl_keys, dr$con, ...)
  }
}

test_that("dbi (postgres via RPostgres)", {
  skip_on_cran()
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    if (has_postgres(RPostgres::Postgres)) {
      storr::test_driver(pg_tester(RPostgres::Postgres))
    }
  }
})

test_that("dbi (postgres via RPostgreSQL)", {
  skip_on_cran()
  if (requireNamespace("RPostgreSQL", quietly = TRUE)) {
    oo <- options(warnPartialMatchArgs = FALSE)
    if (!is.null(oo$warnPartialMatchArgs)) {
      on.exit(options(oo))
    }
    if (has_postgres(RPostgreSQL::PostgreSQL)) {
      storr::test_driver(pg_tester(RPostgreSQL::PostgreSQL))
    }
  }
})

test_that("redis_api", {
  skip_on_cran()
  if (!identical(Sys.getenv("ISOLATED_REDIS"), "true")) {
    skip("No isolated Redis server to work with")
  }
  has_redis <- tryCatch(redux::redis_available(), error = function(e) FALSE)
  if ("redux" %in% .packages(TRUE)) {
    con <- redux::hiredis()
    storr::test_driver(function(dr = NULL, ...)
      driver_redis_api(dr$prefix %||% rand_str(), con, ...))
  }
  ## if ("rrlite" %in% .packages(TRUE)) {
  ##   con <- rrlite::hirlite()
  ##   storr::test_driver(function(dr = NULL, ...)
  ##     driver_redis_api(dr$prefix %||% rand_str(), con, ...))
  ## }
})
