##' Redis object cache driver
##' @title Redis object cache driver
##'
##' @param prefix Prefix for keys.  We'll generate a number of keys
##'   that start with this string.  Probably terminating the string
##'   with a punctuation character (e.g., ":") will make created
##'   strings nicer to deal with.
##'
##' @param con A \code{redis_api} connection object, as created by the
##'   redux or rrlite packages.
##'
##' @param hash_algorithm Name of the hash algorithm to use.  Possible
##'   values are "md5", "sha1", and others supported by
##'   \code{\link{digest}}.  If not given, then we will default to
##'   "md5".
##'
##' @param default_namespace Default namespace (see \code{\link{storr}}).
##' @export
##' @author Rich FitzJohn
storr_redis_api <- function(prefix, con, hash_algorithm = NULL,
                            default_namespace = "objects") {
  storr(driver_redis_api(prefix, con, hash_algorithm), default_namespace)
}

##' @export
##' @rdname storr_redis_api
driver_redis_api <- function(prefix, con, hash_algorithm = NULL) {
  R6_driver_redis_api$new(prefix, con, hash_algorithm)
}

R6_driver_redis_api <- R6::R6Class(
  "driver_redis_api",
  public = list(
    con = NULL,
    prefix = NULL,
    traits = list(accept = "raw", throw_missing = TRUE),
    hash_algorithm = NULL,

    initialize = function(prefix, con, hash_algorithm) {
      self$prefix <- prefix
      self$con <- con
      self$hash_algorithm <- driver_redis_api_config(
        con, prefix, "hash_algorithm", hash_algorithm, "md5", TRUE)
      storr_lua_load(con)
    },
    type = function() {
      paste("redis_api", self$con$type(), sep = "/")
    },
    destroy = function() {
      redis_drop_keys(self$con, paste0(self$prefix, "*"))
      self$con <- NULL
    },

    get_hash = function(key, namespace) {
      res <- self$con$GET(self$name_key(key, namespace))
      if (is.null(res)) {
        stop("No such hash")
      }
      res
    },

    mget_hash = function(key, namespace) {
      dat <- join_key_namespace(key, namespace)
      if (dat$n == 0L) {
        return(character(0))
      }
      res <- self$con$MGET(self$name_key(dat$key, dat$namespace))
      i <- vlapply(res, is.null)
      res[i] <- NA_character_
      unlist(res, use.names = FALSE)
    },

    set_hash = function(key, namespace, hash) {
      self$con$SET(self$name_key(key, namespace), hash)
    },

    mset_hash = function(key, namespace, hash) {
      if (length(hash) == 0L) {
        return()
      }
      self$con$MSET(self$name_key(key, namespace), hash)
    },

    get_object = function(hash) {
      res <- self$con$GET(self$name_hash(hash))
      if (is.null(res)) {
        stop("No such object")
      }
      unserialize(res)
    },

    mget_object = function(hash) {
      if (length(hash) == 0) {
        return(list())
      }
      res <- self$con$MGET(self$name_hash(hash))
      i <- !vlapply(res, is.null)
      res[i] <- lapply(res[i], unserialize)
      res
    },

    set_object = function(hash, value) {
      assert_raw(value)
      self$con$SET(self$name_hash(hash), value)
    },

    mset_object = function(hash, value) {
      ## TODO: probably storr should avoid passing in zero-length
      ## requests for iformation throughout (all four m*et functions).
      if (length(value) == 0L) {
        return()
      }
      self$con$MSET(self$name_hash(hash), value)
    },

    exists_hash = function(key, namespace) {
      mcmd_run(self$con, "EXISTS", self$name_key(key, namespace))
    },
    exists_object = function(hash) {
      mcmd_run(self$con, "EXISTS", self$name_hash(hash))
    },

    del_hash = function(key, namespace) {
      mcmd_run(self$con, "DEL", self$name_key(key, namespace))
    },
    del_object = function(hash) {
      mcmd_run(self$con, "DEL", self$name_hash(hash))
    },

    ## This suggests that dir(), ls(), etc could all work with these in
    ## the same way pretty easily.  But the str_drop_start is a pretty big
    ## assumption.
    list_hashes = function() {
      start <- sprintf("%s:data:%s", self$prefix, "")
      str_drop_start(redis_list_keys(self$con, paste0(start, "*")), start)
    },
    list_keys = function(namespace) {
      start <- self$name_key("", namespace)
      str_drop_start(redis_list_keys(self$con, paste0(start, "*")), start)
    },
    list_namespaces = function() {
      ## For this to work, consider disallowing ":" in namespace
      ## names, or sanitising them on the way in?
      pattern <- self$name_key("*", "*")
      re <- self$name_key(".*", "([^:]*)")
      unique(sub(re, "\\1", redis_list_keys(self$con, pattern)))
    },

    name_hash = function(hash) {
      sprintf("%s:data:%s", self$prefix, hash)
    },
    name_key = function(key, namespace) {
      sprintf("%s:keys:%s:%s", self$prefix, namespace, key)
    }
  ))

redis_drop_keys <- function(con, pattern) {
  del <- redis_list_keys(con, pattern)
  if (length(del) > 0) {
    con$DEL(del)
  }
}
## TODO: Merge into redux with a best-practice based on SCAN, though
## doing that while being able to test for SCAN is hard, and getting
## that to work correctly for rrlite is hard without directly invoking
## the package.  This is a rare operation and only used in tests so it
## should be OK.
##
## TODO: on entry, try to detect if we have SCAN support and drop the
## KEYS call if so.  Or switch on the type.  Or I can try and patch
## rrlite.
redis_list_keys <- function(con, pattern) {
  as.character(con$KEYS(pattern))
}

driver_redis_api_config <- function(con, prefix, name, value, default,
                                    must_agree) {
  path_opt <- sprintf("%s:config:%s", prefix, name)

  load_value <- function() {
    if (con$EXISTS(path_opt)) {
      value <- con$GET(path_opt)
      storage.mode(value) <- storage.mode(default)
    } else {
      value <- default
    }
    value
  }

  if (is.null(value)) {
    value <- load_value()
  } else if (must_agree && con$EXISTS(path_opt)) {
    value_prev <- load_value()
    if (value != value_prev) {
      stop(ConfigError(name, value_prev, value))
    }
  }
  if (!con$EXISTS(path_opt)) {
    con$SET(path_opt, value)
  }

  value
}

## This is an alternative to redux::redis_scripts(), which isolates
## the grossness a bit.  These commands (vectorised delete and exists)
## have basically the same form, so this approach combines them
## together in a single lua script.
"local result = {}
for _, val in pairs(KEYS) do
  result[#result + 1] = redis.call(ARGV[1], val)
end
return result" -> STORR_LUA
STORR_LUA_SHA <- "ef97af5300a280cb6fd597e18899dc2ffb000f96"
storr_lua_load <- function(con) {
  sha <- con$SCRIPT_LOAD(STORR_LUA)
  if (sha != STORR_LUA_SHA) {
    stop("failure in loading load script [storr bug]") # nocov
  }
  sha
}
mcmd_run <- function(con, cmd, keys) {
  if (length(keys) == 1L) {
    con[[cmd]](keys) == 1L
  } else {
    unlist(con$EVALSHA(STORR_LUA_SHA, length(keys), keys, cmd)) == 1L
  }
}
