##' Create an object cache; a "storr".  A storr is a simple key-value
##' store where the actual content is stored in a content-addressible
##' way (so that duplicate objects are only stored once) and with a
##' caching layer so that repeated lookups are fast even if the
##' underlying storage driver is slow.
##'
##' To create a storr you need to provide a "driver" object.  There
##' are three in the package: \code{\link{driver_environment}} for
##' ephemeral in-memory storage, \code{\link{driver_rds}} for
##' serialised storage to disk and \code{\link{driver_redis_api}}
##' which stores data in Redis but requires packages that are not on
##' CRAN to function
##' (\href{https://github.com/ropensci/RedisAPI}{RedisAPI} and one of
##' \href{https://github.com/ropensci/rrlite}{rrlite} or
##' \href{https://github.com/richfitz/redux}{redux}).  New drivers are
##' relatively easy to add -- see the "drivers" vignette
##' (\code{vignette("drivers", package="storr")}).
##'
##' There are convenience functions (e.g.,
##' \code{\link{storr_environment}} and \code{\link{storr_rds}}) that
##' may be more convenient to use than this function.
##'
##' Once a storr has been made it provides a number of methods.
##' Because storr uses \code{R6} (\code{\link{R6Class}}) objects, each
##' method is accessed by using \code{$} on a storr object (see the
##' examples).  The methods are described below in the "Methods"
##' section.
##'
##' The \code{default_namespace} affects all methods of the storr
##' object that refer to namespaces; if a namespace is not given, then
##' the action (get, set, del, list, import, export) will affect the
##' \code{default_namespace}.  By default this is \code{"objects"}.
##'
##' @template storr_methods
##'
##' @title Object cache
##'
##' @param driver A driver object
##'
##' @param default_namespace Default namespace to store objects in.
##'   By default \code{"objects"} is used, but this might be useful to
##'   have two diffent \code{storr} objects pointing at the same
##'   underlying storage, but storing things in different namespaces.
##'
##' @export
##' @examples
##' st <- storr(driver_environment())
##' ## Set "mykey" to hold the mtcars dataset:
##' st$set("mykey", mtcars)
##' ## and get the object:
##' st$get("mykey")
##' ## List known keys:
##' st$list()
##' ## List hashes
##' st$list_hashes()
##' ## List keys in another namespace:
##' st$list("namespace2")
##' ## We can store things in other namespaces:
##' st$set("x", mtcars, "namespace2")
##' st$set("y", mtcars, "namespace2")
##' st$list("namespace2")
##' ## Duplicate data do not cause duplicate storage: despite having three
##' ## keys we only have one bit of data:
##' st$list_hashes()
##' st$del("mykey")
##'
##' ## Storr objects can be created that have a default namespace that is
##' ## not "objects" by using the \code{default_namespace} argument (this
##' ## one also points at the same memory as the first storr).
##' st2 <- storr(driver_environment(st$driver$envir),
##'              default_namespace="namespace2")
##' ## All functions now use "namespace2" as the default namespace:
##' st2$list()
##' st2$del("x")
##' st2$del("y")
storr <- function(driver, default_namespace="objects") {
  .R6_storr$new(driver, default_namespace)
}

## NOTE: without some fairly ugly gymnastics, this is a **build-time**
## dependency on R6, rather than a runtime dependency, so the import
## directive is required to avoid a NOTE.  The alternative is to set
## up dummy bindings, make generating functions and set the bindings
## during .onLoad(); that's not ideal though because it makes the code
## a lot more obfuscated.
##' @importFrom R6 R6Class
.R6_storr <- R6::R6Class(
  "storr",
  public=list(
    driver=NULL,
    envir=NULL,
    default_namespace=NULL,
    traits=NULL,
    initialize=function(driver, default_namespace) {
      self$driver <- driver
      self$envir <- new.env(parent=emptyenv())
      self$default_namespace <- default_namespace
      self$traits <- storr_traits(driver$traits)
    },

    destroy=function() {
      self$driver$destroy()
      self$driver <- NULL
    },

    flush_cache=function() {
      rm(list=ls(self$envir, all.names=TRUE), envir=self$envir)
    },

    set=function(key, value, namespace=self$default_namespace, use_cache=TRUE) {
      hash <- self$set_value(value, use_cache)
      self$driver$set_hash(key, namespace, hash)
      invisible(hash)
    },

    set_by_value=function(value, namespace=self$default_namespace, use_cache=TRUE) {
      hash <- self$set_value(value, use_cache)
      self$driver$set_hash(hash, namespace, hash)
      invisible(hash)
    },

    get=function(key, namespace=self$default_namespace, use_cache=TRUE) {
      self$get_value(self$get_hash(key, namespace), use_cache)
    },

    get_hash=function(key, namespace=self$default_namespace) {
      if (self$traits$throw_missing) {
        tryCatch(self$driver$get_hash(key, namespace),
                 error=function(e) stop(KeyError(key, namespace)))
      } else {
        if (self$exists(key, namespace)) {
          self$driver$get_hash(key, namespace)
        } else {
          stop(KeyError(key, namespace))
        }
      }
    },

    del=function(key, namespace=self$default_namespace) {
      invisible(self$driver$del_hash(key, namespace))
    },
    clear=function(namespace=self$default_namespace) {
      if (is.null(namespace)) {
        invisible(sum(viapply(self$list_namespaces(), self$clear)))
      } else {
        invisible(length(vlapply(self$list(namespace), self$del, namespace)))
      }
    },
    exists=function(key, namespace=self$default_namespace) {
      self$driver$exists_hash(key, namespace)
    },

    exists_object=function(hash) {
      self$driver$exists_object(hash)
    },

    gc=function() {
      storr_gc(self$driver, self$envir)
    },

    ## TODO: Allow drivers to declare that they will throw on
    ## invalid access to save two lookups here.
    get_value=function(hash, use_cache=TRUE) {
      envir <- self$envir
      if (use_cache && exists0(hash, envir)) {
        value <- envir[[hash]]
      } else {
        if (self$traits$throw_missing) {
          value <- tryCatch(self$driver$get_object(hash),
                            error=function(e) stop(HashError(hash)))
        } else {
          if (!self$driver$exists_object(hash)) {
            stop(HashError(hash))
          }
          value <- self$driver$get_object(hash)
        }
        if (use_cache) {
          envir[[hash]] <- value
        }
      }
      value
    },

    set_value=function(value, use_cache=TRUE) {
      if (self$traits$accept_raw) {
        value_dr <- serialize(value, NULL)
        hash <- hash_object(value_dr, serialize=FALSE, skip=14L)
      } else {
        value_dr <- value
        hash <- hash_object(value)
      }

      ## NOTE: This exists/set roundtrip here always seems useful to
      ## avoid sending (potentially large) data over a connection.
      if (!self$driver$exists_object(hash)) {
        self$driver$set_object(hash, value_dr)
      }
      if (use_cache && !exists0(hash, self$envir)) {
        assign(hash, value, self$envir)
      }
      invisible(hash)
    },

    ## We guarantee key sort here; underlying driver does not have to.
    list=function(namespace=self$default_namespace) {
      sort(self$driver$list_keys(namespace))
    },

    list_hashes=function() {
      sort(self$driver$list_hashes())
    },

    list_namespaces=function() {
      sort(self$driver$list_namespaces())
    },

    ## To/from R environments (distinct from the environment driver)
    import=function(src, list=NULL, namespace=self$default_namespace) {
      storr_copy(self, src, list, namespace)$names
    },

    ## The logic here is taken from remake's object_store, which is
    ## useful as this is destined to replace that object.
    export=function(dest, list=NULL, namespace=self$default_namespace) {
      invisible(storr_copy(dest, self, list, namespace)$dest)
    },

    ## TODO: Deal with all the namespaces at once perhaps, or at least
    ## allow a vector of namespaces here.  The place to implement this
    ## would be in storr_copy because it would flow through
    ## everything else.
    archive_export=function(path, names=NULL,
                            namespace=self$default_namespace) {
      self$export(storr_rds(path, mangle_key=TRUE), names, namespace)
    },

    archive_import=function(path, names=NULL,
                            namespace=self$default_namespace) {
      self$import(storr_rds(path, mangle_key=TRUE), names, namespace)
    }))

## This one is complicated enough to come out.
storr_gc <- function(driver, envir) {
  ns <- driver$list_namespaces()
  hashes <- driver$list_hashes()
  seen <- character(0)
  f <- function(key, namespace) {
    driver$get_hash(key, namespace)
  }
  seen <- unique(unlist(lapply(ns, function(i)
    unique(vcapply(driver$list_keys(i), f, i)))))
  unused <- setdiff(hashes, seen)
  for (h in unused) {
    driver$del_object(h)
  }
  rm0(unused, envir)
  invisible(unused)
}
