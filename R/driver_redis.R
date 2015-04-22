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
    key_hash_value=NULL,
    key_key_hash=NULL,

    initialize=function(prefix, con) {
      self$con <- con
      ## hash -> data mapping
      self$key_hash_value <- paste0(prefix, ":data")
      ## key -> hash mapping
      self$key_key_hash <- paste0(prefix, ":hash")
    },

    exists_hash=function(hash) {
      self$con$HEXISTS(self$key_hash_value, hash) == 1L
    },
    exists_key=function(key) {
      self$con$HEXISTS(self$key_key_hash, key) == 1L
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      ## This is not ideal because we'll have hashed the object twice
      ## to get here, but I don't see how else to get around it...
      str <- object_to_string(value)
      self$con$HSET(self$key_hash_value, hash, str)
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash) {
      self$con$HSET(self$key_key_hash, key, hash)
    },

    ## Get value, given hash
    get_value=function(hash) {
      if (self$exists_hash(hash)) {
        self$con$HGET(self$key_hash_value, hash)
      } else {
        stop(HashError(hash))
      }
    },
    ## Get hash, given key
    get_hash=function(key) {
      if (self$exists_key(key)) {
        self$con$HGET(self$key_key_hash, key)
      } else {
        stop(KeyError(key))
      }
    },

    list_hashes=function() {
      as.character(self$con$HKEYS(self$key_hash_value))
    },
    list_keys=function() {
      as.character(self$con$HKEYS(self$key_key_hash))
    },

    del_hash=function(hash) {
      self$con$HDEL(self$key_hash_value, hash) == 1L
    },
    del_key=function(key) {
      self$con$HDEL(self$key_key_hash, key) == 1L
    }
  ))
