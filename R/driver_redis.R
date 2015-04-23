##' Redis object cache driver
##' @title Redis object cache driver
##' @param prefix Prefix for keys
##' @param ... Arguments passed through to \code{RedisAPI::hiredis} or
##' \code{rrlite::hirlite}.
##' @export
##' @author Rich FitzJohn
driver_redis <- function(prefix, ...) {
  driver_redis_api(prefix, RedisAPI::hiredis(...))
}
##' @export
##' @rdname driver_redis
driver_rlite <- function(prefix, ...) {
  driver_redis_api(prefix, rrlite::hirlite(...))
}

driver_redis_api <- function(prefix, con) {
  .R6_driver_redis_api$new(prefix, con)
}

##' @importFrom RedisAPI object_to_string string_to_object
.R6_driver_redis_api <- R6::R6Class(
  "driver_redis_api",

  public=list(
    con=NULL,
    prefix_data=NULL,
    prefix_keys=NULL,

    initialize=function(prefix, con) {
      self$con <- con
      ## hash -> data mapping
      self$prefix_data <- paste0(prefix, ":data")
      ## key -> hash mapping
      self$prefix_keys <- paste0(prefix, ":hash")
    },

    exists_hash=function(hash) {
      self$con$EXISTS(self$name_data(hash)) == 1L
    },
    exists_key=function(key) {
      self$con$EXISTS(self$name_key(key)) == 1L
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      self$con$SET(self$name_data(hash), object_to_string(value))
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash) {
      self$con$SET(self$name_key(key), hash)
    },

    ## Get value, given hash
    get_value=function(hash) {
      name <- self$name_data(hash)
      if (self$con$EXISTS(name)) {
        string_to_object(self$con$GET(name))
      } else {
        stop(HashError(hash))
      }
    },
    ## Get hash, given key
    get_hash=function(key) {
      name <- self$name_key(key)
      if (self$con$EXISTS(name)) {
        self$con$GET(name)
      } else {
        stop(KeyError(key))
      }
    },

    del_hash=function(hash) {
      self$con$DEL(self$name_data(hash)) == 1L
    },
    del_key=function(key) {
      self$con$DEL(self$name_key(key)) == 1L
    },

    ## Potentially expensive!
    list_hashes=function() {
      keys_minus_prefix(self$con, self$name_data(""))
    },
    list_keys=function() {
      keys_minus_prefix(self$con, self$name_key(""))
    },

    name_data=function(hash) {
      sprintf("%s:%s", self$prefix_data, hash)
    },
    name_key=function(key) {
      sprintf("%s:%s", self$prefix_keys, key)
    }
  ))
