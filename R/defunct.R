##' Defunct functions
##'
##' The redis functions (\code{driver_redis_api} and
##' \code{storr_redis_api}) have been moved out of this package and
##' into Redis.  I don't believe anyone is using them at the time of
##' the move so this is being done fairly abruptly - this is
##' unfortunate, but necessary to avoid a circular dependency!  The
##' new functions are simply \code{redux::driver_redis_api} and
##' \code{redux::storr_redis_api}, along with a helper function
##' \code{redux::storr_hiredis} which also creates the connection.
##'
##' @title Defunct functions
##' @param ... parameters (now all dropped as dots)
##' @rdname storr-defunct
##' @export
driver_redis_api <- function(...) {
  .Defunct("redux::driver_redis_api")
}

##' @rdname storr-defunct
##' @export
storr_redis_api <- function(...) {
  .Defunct("redux::storr_redis_api")
}
