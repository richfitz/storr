create_drivers <- function() {
  path <- tempfile()
  prefix <- "storr"
  ret <- list(envir=driver_environment(),
              rds=driver_rds(path),
              redis=driver_redis(prefix),
              rlite=driver_rlite(prefix))
  attr(ret, "cleanup") <- function() {
    unlink(path, recursive=TRUE)
    drop_keys(RedisAPI::hiredis(), paste0(prefix, "*"))
  }
  ret
}

cleanup_drivers <- function(drivers) {
  attr(drivers, "cleanup")()
}

equals_unsorted <- function(expected, ...) {
  eq <- testthat::equals(sort(expected), ...)
  function(actual) {
    eq(sort(actual))
  }
}

skip_if_no_downloads <- function() {
  skip_unless_internet()
  if (Sys.getenv("STORR_SKIP_DOWNLOADS") == "") {
    return()
  }
  skip("Skipping downloads")
}

skip_unless_internet <- function() {
  if (has_internet()) {
    return()
  }
  skip("No internet :(")
}

has_internet <- function() {
  !is.null(suppressWarnings(nsl("www.google.com")))
}
