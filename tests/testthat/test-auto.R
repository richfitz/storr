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
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    if (has_postgres(RPostgres::Postgres)) {
      storr::test_driver(pg_tester(RPostgres::Postgres))
    }
  }
})

test_that("dbi (postgres via RPostgreSQL)", {
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
