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

drop_keys <- function(con, pattern) {
  del <- as.character(con$KEYS(pattern))
  if (length(del) > 0) {
    con$DEL(del)
  }
}
