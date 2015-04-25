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
    prefix=NULL,

    initialize=function(prefix, con) {
      self$con <- con
      ## hash -> data mapping
      ## key -> hash mapping
      ## list element -> hash mapping
      self$prefix <- prefix
    },

    flush=function() {
      drop_keys(self$con, paste0(self$prefix, "*"))
    },

    exists_hash=function(hash) {
      self$con$EXISTS(self$name_data(hash)) == 1L
    },
    exists_key=function(key, namespace) {
      self$con$EXISTS(self$name_key(key, namespace)) == 1L
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      self$con$SET(self$name_data(hash), object_to_string(value))
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash, namespace) {
      self$con$SET(self$name_key(key, namespace), hash)
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
    get_hash=function(key, namespace) {
      name <- self$name_key(key, namespace)
      if (self$con$EXISTS(name)) {
        self$con$GET(name)
      } else {
        stop(KeyError(key))
      }
    },

    del_hash=function(hash) {
      self$con$DEL(self$name_data(hash)) == 1L
    },
    del_key=function(key, namespace) {
      self$con$DEL(self$name_key(key, namespace)) == 1L
    },

    ## Potentially expensive!
    list_hashes=function() {
      keys_minus_prefix(self$con, self$name_data(""))
    },
    list_keys=function(namespace) {
      keys_minus_prefix(self$con, self$name_key("", namespace))
    },

    ## List support:
    is_list=function(key, namespace) {
      self$con$EXISTS(self$name_list(key, namespace)) == 1L
    },
    length_list=function(key, namespace) {
      self$con$LLEN(self$name_list(key, namespace))
    },

    set_key_hash_list=function(key, i, hash, namespace) {
      name <- self$name_list(key, namespace)
      if (is.null(i)) {
        self$con$DEL(name)
        self$con$RPUSH(name, hash)
      } else if (self$is_list(key, namespace)) {
        list_check_range(key, i, self$length_list(key, namespace))
        for (j in i) {
          self$con$LSET(name, i - 1L, hash[[j]])
        }
      } else {
        stop(TypeError(key, "list", self$con$TYPE(key)))
      }
    },

    ## TODO: Getting good error messages without a billion calls back
    ## to Redis is going to be difficult here.  Can fail because:
    ##   - key does not exist (KeyError?)
    ##   - key is not a list (TypeError?)
    ##   - index is out of bounds (RangeError?)
    get_hash_list=function(key, i, namespace) {
      name <- self$name_list(key, namespace)
      if (is.null(i)) {
        as.character(self$con$LRANGE(name, 0, -1))
      } else {
        list_check_range(key, i, self$length_list(key, namespace))
        vcapply(i - 1L, self$con$LINDEX, key=name)
      }
    },
    del_hash_list=function(key, namespace) {
      self$con$DEL(self$name_list(key, namespace))
    },

    name_data=function(hash) {
      sprintf("%s:data:%s", self$prefix, hash)
    },
    name_key=function(key, namespace) {
      sprintf("%s:keys:%s:%s", self$prefix, namespace, key)
    },
    name_list=function(key, namespace) {
      sprintf("%s:lists:%s:%s", self$prefix, namespace, key)
    }
  ))

## TODO: Merge into RedisAPI with a best-practice based on SCAN.
drop_keys <- function(con, pattern) {
  del <- as.character(con$KEYS(pattern))
  if (length(del) > 0) {
    con$DEL(del)
  }
}
