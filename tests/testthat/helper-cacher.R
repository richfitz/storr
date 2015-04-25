create_drivers <- function() {
  path <- tempfile()
  prefix <- "cacher"
  ret <- list(envir=driver_environment(),
              rds=driver_rds(path),
              driver=driver_redis(prefix),
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
