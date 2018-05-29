##' Fast but transient environment driver.  This driver saves objects
##' in a local R environment, without serialisation.  This makes
##' lookup fast but it cannot be saved across sesssions.  The
##' environment storr can be made persistent by saving it out as a
##' file storr though.
##'
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
##' @param hash_algorithm Name of the hash algorithm to use.  Possible
##'   values are "md5", "sha1", and others supported by
##'   \code{\link{digest}}.  If not given, then we will default to
##'   "md5".
##'
##' @export
##' @examples
##'
##' # Create an environment and stick some random numbers into it:
##' st <- storr_environment()
##' st$set("foo", runif(10))
##' st$get("foo")
##'
##' # To make this environment persistent we can save it to disk:
##' path <- tempfile()
##' st2 <- st$archive_export(path)
##' # st2 is now a storr_rds (see ?storr_rds), and will persist across
##' # sessions.
##'
##' # or export to a new list:
##' lis <- st$export(list())
##' lis
storr_environment <- function(envir = NULL, hash_algorithm = NULL,
                              default_namespace = "objects") {
  storr(driver_environment(envir, hash_algorithm), default_namespace)
}


##' @export
##' @rdname storr_environment
driver_environment <- function(envir = NULL, hash_algorithm = NULL) {
  if (is.null(envir)) {
    envir <- new.env(parent = emptyenv())
  }
  R6_driver_environment$new(envir, hash_algorithm)
}


R6_driver_environment <- R6::R6Class(
  "driver_environment",
  public = list(
    envir = NULL,
    hash_algorithm = NULL,
    traits = list(accept = "object"),

    initialize = function(envir, hash_algorithm) {
      if (!is.null(hash_algorithm)) {
        assert_scalar_character(hash_algorithm)
      }
      if (is.null(envir$data)) {
        envir$keys <- list()
        envir$data <- new.env(parent = emptyenv())
        envir$hash_algorithm <- hash_algorithm <- hash_algorithm %||% "md5"
      } else {
        assert_environment(envir$data)
        assert_list(envir$keys)
        if (is.null(hash_algorithm)) {
          hash_algorithm <- envir$hash_algorithm
        } else {
          if (hash_algorithm != envir$hash_algorithm) {
            stop(ConfigError("hash_algorithm", envir$hash_algorithm,
                             hash_algorithm))
          }
        }
      }
      self$envir <- envir
      self$hash_algorithm <- hash_algorithm
    },

    type = function() {
      "environment"
    },

    destroy = function() {
      self$envir$keys <- NULL
      self$envir$list <- NULL
      self$envir$data <- NULL
      self$envir <- NULL
    },

    get_hash = function(key, namespace) {
      self$ensure_envir(namespace)[[key]]
    },

    set_hash = function(key, namespace, hash) {
      e <- self$ensure_envir(namespace)
      e[[key]] <- hash
    },

    get_object = function(hash) {
      self$envir$data[[hash]]
    },

    set_object = function(hash, value) {
      self$envir$data[[hash]] <- value
    },

    exists_hash = function(key, namespace) {
      kn <- join_key_namespace(key, namespace)
      key <- kn$key
      namespace <- kn$namespace
      f <- function(k, n) {
        e <- self$envir$keys[[n]]
        is.environment(e) && exists0(k, e)
      }
      vlapply(seq_along(key), function(i) f(key[[i]], namespace[[i]]))
    },

    exists_object = function(hash) {
      vlapply(hash, exists0, self$envir$data, USE.NAMES = FALSE)
    },

    del_hash = function(key, namespace) {
      kn <- join_key_namespace(key, namespace)

      idx <- split(seq_len(kn$n), kn$namespace)
      ret <- logical(kn$n)

      for (i in seq_along(idx)) {
        e <- self$envir$keys[[names(idx)[[i]]]]
        if (is.environment(e)) {
          j <- idx[[i]]
          ret[j] <- rm0(kn$key[j], e)
        }
      }

      ret
    },
    del_object = function(hash) {
      rm0(hash, self$envir$data)
    },

    list_hashes = function() {
      ls(self$envir$data)
    },

    list_keys = function(namespace) {
      e <- self$ensure_envir(namespace)
      ls(e)
    },

    list_namespaces = function() {
      as.character(names(self$envir$keys))
    },

    ensure_envir = function(namespace) {
      force(namespace) # avoid obscure missing argument error
      ret <- self$envir$keys[[namespace]]
      if (is.null(ret)) {
        ret <- self$envir$keys[[namespace]] <- new.env(parent = emptyenv())
      }
      ret
    }
  ))
