##' Dummy environment driver that can possibly be used for the internals
##' @title Environment object cache driver
##' @export
driver_environment <- function() {
  .R6_driver_environment$new()
}

## TODO: get / [[
## TODO: assign / [[<-
.R6_driver_environment <- R6::R6Class(
  "driver_environment",

  public=list(
    envir_data=NULL,
    envir_keys=NULL,
    envir_list=NULL,

    initialize=function() {
      self$envir_data <- new.env(parent=emptyenv())
      self$envir_keys <- list()
      self$envir_list <- list()
    },

    exists_hash=function(hash) {
      exists0(hash, self$envir_data)
    },
    exists_key=function(key, namespace) {
      self$ensure_envir("envir_keys", namespace)
      exists0(key, self$envir_keys[[namespace]])
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      assign(hash, value, self$envir_data)
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash, namespace) {
      self$ensure_envir("envir_keys", namespace)
      assign(key, hash, self$envir_keys[[namespace]])
    },

    ## Get value, given hash
    get_value=function(hash) {
      if (self$exists_hash(hash)) {
        self$envir_data[[hash]]
      } else {
        stop(HashError(hash))
      }
    },
    ## Get hash, given key
    get_hash=function(key, namespace) {
      if (self$exists_key(key, namespace)) {
        self$envir_keys[[namespace]][[key]]
      } else {
        stop(KeyError(key))
      }
    },

    del_hash=function(hash) {
      rm0(hash, self$envir_data)
    },
    del_key=function(key, namespace) {
      self$ensure_envir("envir_keys", namespace)
      rm0(key, self$envir_keys[[namespace]])
    },

    list_hashes=function() {
      ls(self$envir_data)
    },
    list_keys=function(namespace) {
      self$ensure_envir("envir_keys", namespace)
      ls(self$envir_keys[[namespace]])
    },

    ## List support:
    is_list=function(key, namespace) {
      self$ensure_envir("envir_list", namespace)
      exists0(key, self$envir_list[[namespace]])
    },
    length_list=function(key, namespace) {
      length(self$envir_list[[namespace]][[key]])
    },

    set_key_hash_list=function(key, i, hash, namespace) {
      self$ensure_envir("envir_list", namespace)

      if (is.null(i)) {
        assign(key, hash, self$envir_list[[namespace]])
      } else if (self$is_list(key, namespace)) {
        list_check_range(key, i, self$length_list(key, namespace))
        tmp <- self$envir_list[[namespace]][[key]]
        tmp[i] <- hash
        assign(key, hash, self$envir_list[[namespace]])
      } else {
        ## TODO: driver-level type information - none or string here?
        stop(TypeError(key, "list", "other"))
      }
    },

    get_hash_list=function(key, i, namespace) {
      hash <- self$envir_list[[namespace]][[key]]
      if (is.null(hash)) {
        hash <- character(0)
      }
      if (is.null(i)) {
        hash
      } else {
        list_check_range(key, i, length(hash))
        hash[i]
      }
    },
    del_hash_list=function(key, namespace) {
      self$ensure_envir("envir_list", namespace)
      rm0(key, self$envir_list[[namespace]])
    },

    ensure_envir=function(name, namespace) {
      if (is.null(self[[name]][[namespace]])) {
        self[[name]][[namespace]] <- new.env(parent=emptyenv())
      }
    }
  ))
