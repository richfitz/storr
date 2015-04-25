##' Dummy environment driver that can possibly be used for the internals
##' @title Environment object cache driver
##' @export
driver_environment <- function() {
  .R6_driver_environment$new()
}

.R6_driver_environment <- R6::R6Class(
  "driver_environment",

  public=list(
    envir_data=NULL,
    envir_keys=NULL,

    initialize=function() {
      self$envir_data <- new.env(parent=emptyenv())
      self$envir_keys <- list(objects=new.env(parent=emptyenv()))
    },

    exists_hash=function(hash) {
      exists0(hash, self$envir_data)
    },
    exists_key=function(key, namespace) {
      exists0(key, self$envir_keys[[namespace]])
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      assign(hash, value, self$envir_data)
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash, namespace) {
      if (is.null(self$envir_keys[[namespace]])) {
        self$envir_keys[[namespace]] <- new.env(parent=emptyenv())
      }
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
      rm0(key, self$envir_keys[[namespace]])
    },

    list_hashes=function() {
      ls(self$envir_data)
    },
    list_keys=function(namespace) {
      ls(self$envir_keys[[namespace]])
    }
  ))
