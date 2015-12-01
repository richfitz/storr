##' Dummy environment driver that can possibly be used for the internals
##' @title Environment object cache driver
##'
##' @param envir The environment to point the storr at.  The default
##'   creates an new empty environment which is generally the right
##'   choice.  However, if you want multiple environment storrs
##'   pointing at the same environment then pass the \code{envir}
##'   argument along.
##'
##' @export
driver_environment <- function(envir=NULL) {
  if (is.null(envir)) {
    envir <- new.env(parent=emptyenv())
  }
  .R6_driver_environment$new(envir)
}
##' @export
##' @rdname driver_environment
##' @param default_namespace Default namespace (see \code{\link{storr}})
##' @param mangle_key Mangle key? (see \code{\link{storr}})
storr_environment <- function(envir=NULL, default_namespace="objects",
                              mangle_key=FALSE) {
  storr(driver_environment(envir), default_namespace, mangle_key)
}

## TODO: Something that makes it possible for two environment storrs
## to share the same root environment.  This is set up in a fairly
## complicated way though.
.R6_driver_environment <- R6::R6Class(
  "driver_environment",

  public=list(
    envir=NULL,

    initialize=function(envir) {
      if (is.null(envir)) {
        envir <- new.env(parent=emptyenv())
      } else {
        assert_environment(envir)
      }
      self$envir <- envir
      if (is.null(self$envir$data)) {
        self$envir$keys <- list()
        self$envir$list <- list()
        self$envir$data <- new.env(parent=emptyenv())
      } else {
        ## NOTE: not using self for mildly nicer error messages.
        assert_environment(envir$data)
        assert_list(envir$keys)
        assert_list(envir$list)
      }
    },
    destroy=function() {
      self$envir <- NULL
    },
    copy=function() {
      driver_environment(self$envir)
    },

    exists_hash=function(hash) {
      exists0(hash, self$envir$data)
    },
    exists_key=function(key, namespace) {
      self$ensure_envir("keys", namespace)
      exists0(key, self$envir$keys[[namespace]])
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      assign(hash, value, self$envir$data)
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash, namespace) {
      self$ensure_envir("keys", namespace)
      assign(key, hash, self$envir$keys[[namespace]])
    },

    ## Get value, given hash
    get_value=function(hash) {
      if (self$exists_hash(hash)) {
        self$envir$data[[hash]]
      } else {
        stop(HashError(hash))
      }
    },
    ## Get hash, given key
    get_hash=function(key, namespace) {
      if (self$exists_key(key, namespace)) {
        self$envir$keys[[namespace]][[key]]
      } else {
        stop(KeyError(key))
      }
    },

    del_hash=function(hash) {
      rm0(hash, self$envir$data)
    },
    del_key=function(key, namespace) {
      self$ensure_envir("keys", namespace)
      rm0(key, self$envir$keys[[namespace]])
    },

    list_hashes=function() {
      ls(self$envir$data)
    },
    list_keys=function(namespace) {
      self$ensure_envir("keys", namespace)
      ls(self$envir$keys[[namespace]])
    },

    ## List support:
    exists_list=function(key, namespace) {
      self$ensure_envir("list", namespace)
      exists0(key, self$envir$list[[namespace]])
    },
    list_lists=function(namespace) {
      self$ensure_envir("list", namespace)
      ls(self$envir$list[[namespace]])
    },
    length_list=function(key, namespace) {
      length(self$envir$list[[namespace]][[key]])
    },

    set_key_hash_list=function(key, i, hash, namespace) {
      self$ensure_envir("list", namespace)

      if (is.null(i)) {
        assign(key, hash, self$envir$list[[namespace]])
      } else if (self$exists_list(key, namespace)) {
        list_check_range(key, i, self$length_list(key, namespace))
        tmp <- self$envir$list[[namespace]][[key]]
        tmp[i] <- hash
        assign(key, tmp, self$envir$list[[namespace]])
      } else {
        ## TODO: driver-level type information - none or string here?
        stop(TypeError(key, "list", "other"))
      }
    },

    get_hash_list=function(key, i, namespace) {
      hash <- self$envir$list[[namespace]][[key]]
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
      self$ensure_envir("list", namespace)
      rm0(key, self$envir$list[[namespace]])
    },

    ensure_envir=function(name, namespace) {
      force(namespace) # avoid obscure missing argument error
      if (is.null(self$envir[[name]][[namespace]])) {
        self$envir[[name]][[namespace]] <- new.env(parent=emptyenv())
      }
    }
  ))
