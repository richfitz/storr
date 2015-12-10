##' Fast but transient environment driver
##' @title Environment object cache driver
##'
##' @param envir The environment to point the storr at.  The default
##'   creates an new empty environment which is generally the right
##'   choice.  However, if you want multiple environment storrs
##'   pointing at the same environment then pass the \code{envir}
##'   argument along.
##'
##' @param default_namespace Default namespace (see \code{\link{storr}}).
##'
##' @export
storr_environment <- function(envir=NULL, default_namespace="objects") {
  storr(driver_environment(envir), default_namespace)
}

##' @export
##' @rdname storr_environment
driver_environment <- function(envir=NULL) {
  if (is.null(envir)) {
    envir <- new.env(parent=emptyenv())
  }
  .R6_driver_environment$new(envir)
}

.R6_driver_environment <- R6::R6Class(
  "driver_environment",
  public=list(
    envir=NULL,

    initialize=function(envir) {
      if (is.null(envir$data)) {
        envir$keys <- list()
        envir$data <- new.env(parent=emptyenv())
      } else {
        assert_environment(envir$data)
        assert_list(envir$keys)
      }
      self$envir <- envir
    },

    type=function() {
      "environment"
    },
    destroy=function() {
      self$envir$keys <- NULL
      self$envir$list <- NULL
      self$envir$data <- NULL
      self$envir <- NULL
    },

    get_hash=function(key, namespace) {
      self$ensure_envir(namespace)[[key]]
    },
    set_hash=function(key, namespace, hash) {
      e <- self$ensure_envir(namespace)
      e[[key]] <- hash
    },
    get_object=function(hash) {
      self$envir$data[[hash]]
    },
    set_object=function(hash, value) {
      self$envir$data[[hash]] <- value
    },

    exists_key=function(key, namespace) {
      exists0(key, self$ensure_envir(namespace))
    },
    exists_hash=function(hash) {
      exists0(hash, self$envir$data)
    },

    del_key=function(key, namespace) {
      rm0(key, self$ensure_envir(namespace))
    },
    del_hash=function(hash) {
      rm0(hash, self$envir$data)
    },

    list_hashes=function() {
      ls(self$envir$data)
    },
    list_keys=function(namespace) {
      ls(self$ensure_envir(namespace))
    },
    list_namespaces=function() {
      as.character(names(self$envir$keys))
    },

    ensure_envir=function(namespace) {
      force(namespace) # avoid obscure missing argument error
      ret <- self$envir$keys[[namespace]]
      if (is.null(ret)) {
        ret <- self$envir$keys[[namespace]] <- new.env(parent=emptyenv())
      }
      ret
    }
  ))
