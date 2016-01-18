##' Redis object cache driver
##' @title Redis object cache driver
##'
##' @param prefix Prefix for keys.  We'll generate a number of keys
##'   that start with this string.  Probably terminating the string
##'   with a punctuation character (e.g., ":") will make created
##'   strings nicer to deal with.
##'
##' @param con A \code{redis_api} connection object, as created by the
##'   RedisAPI package.  This package does not actually provide the
##'   tools to create a connection; you need to provide one that is
##'   good to go.
##'
##' @param default_namespace Default namespace (see \code{\link{storr}}).
##' @export
##' @author Rich FitzJohn
storr_redis_api <- function(prefix, con, default_namespace="objects") {
  storr(driver_redis_api(prefix, con), default_namespace)
}

##' @export
##' @rdname storr_redis_api
driver_redis_api <- function(prefix, con) {
  .R6_driver_redis_api$new(prefix, con)
}

.R6_driver_redis_api <- R6::R6Class(
  "driver_redis_api",
  public=list(
    con=NULL,
    prefix=NULL,
    traits=list(accept_raw=TRUE, throw_missing=TRUE),
    initialize=function(prefix, con) {
      self$prefix <- prefix
      self$con <- con
    },
    type=function() {
      paste("redis_api", self$con$type(), sep="/")
    },
    destroy=function() {
      redis_drop_keys(self$con, paste0(self$prefix, "*"))
      self$con <- NULL
    },

    get_hash=function(key, namespace) {
      res <- self$con$GET(self$name_key(key, namespace))
      if (is.null(res)) {
        stop("No such hash")
      }
      res
    },
    set_hash=function(key, namespace, hash) {
      self$con$SET(self$name_key(key, namespace), hash)
    },
    get_object=function(hash) {
      res <- self$con$GET(self$name_hash(hash))
      if (is.null(res)) {
        stop("No such object")
      }
      unserialize(res)
    },
    set_object=function(hash, value) {
      assert_raw(value)
      self$con$SET(self$name_hash(hash), value)
    },

    exists_hash=function(key, namespace) {
      self$con$EXISTS(self$name_key(key, namespace)) == 1L
    },
    exists_object=function(hash) {
      self$con$EXISTS(self$name_hash(hash)) == 1L
    },

    del_hash=function(key, namespace) {
      self$con$DEL(self$name_key(key, namespace)) == 1L
    },
    del_object=function(hash) {
      self$con$DEL(self$name_hash(hash)) == 1L
    },

    ## This suggests that dir(), ls(), etc could all work with these in
    ## the same way pretty easily.  But the str_drop_start is a pretty big
    ## assumption.
    list_hashes=function() {
      start <- sprintf("%s:data:%s", self$prefix, "")
      str_drop_start(redis_list_keys(self$con, paste0(start, "*")), start)
    },
    list_keys=function(namespace) {
      start <- self$name_key("", namespace)
      str_drop_start(redis_list_keys(self$con, paste0(start, "*")), start)
    },
    list_namespaces=function() {
      ## For this to work, consider disallowing ":" in namespace
      ## names, or sanitising them on the way in?
      pattern <- self$name_key("*", "*")
      re <- self$name_key(".*", "([^:]*)")
      unique(sub(re, "\\1", redis_list_keys(self$con, pattern)))
    },

    name_hash=function(hash) {
      sprintf("%s:data:%s", self$prefix, hash)
    },
    name_key=function(key, namespace) {
      sprintf("%s:keys:%s:%s", self$prefix, namespace, key)
    }
  ))

redis_drop_keys <- function(con, pattern) {
  del <- redis_list_keys(con, pattern)
  if (length(del) > 0) {
    con$DEL(del)
  }
}
## TODO: Merge into RedisAPI with a best-practice based on SCAN,
## though doing that while being able to test for SCAN is hard, and
## getting that without invoking RedisAPI is harder.  This is a rare
## operation and only used in tests so it should be OK.
##
## TODO: on entry, try to detect if we have SCAN support and drop the
## KEYS call if so.  Or switch on the type.  Or I can try and patch
## rrlite.
redis_list_keys <- function(con, pattern) {
  as.character(con$KEYS(pattern))
}
