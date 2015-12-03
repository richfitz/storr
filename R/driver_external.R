##' storr driver for fetching external resources
##' @title Driver for fetching exernal resources
##' @param storage_driver Another \code{storr} driver to handle the
##' actual storage.
##' @param fetch_hook A function to run to fetch data when a key is
##' not found in the store.
##' @export
driver_external <- function(storage_driver, fetch_hook) {
  .R6_driver_external$new(storage_driver, fetch_hook)
}

##' @export
##' @rdname driver_external
##' @param default_namespace Default namespace (see \code{\link{storr}})
##' @param mangle_key Mangle key? (see \code{\link{storr}})
storr_external <- function(storage_driver, fetch_hook,
                           default_namespace="objects", mangle_key=FALSE) {
  storr(driver_external(storage_driver, fetch_hook),
        default_namespace, mangle_key)
}

## TODO: Support listing possible keys in externals.

## TODO: Support "expiring" external data sources:
## - time to expire (specified in seconds but also with some more
##   user friendly way but probably not lubridate)
## - support soft / hard expiring: for soft expiring we'll try and
##   refetch but fall back on old values with a warning if we can't
##   get it (this is the dockertest model).  For hard expiry we'll
##   just delete the key before even trying anything.
.R6_driver_external <- R6::R6Class(
  "driver_external",
  lock_objects=FALSE,

  public=list(
    storage_driver=NULL,
    fetch_hook=NULL,

    initialize=function(storage_driver, fetch_hook) {
      check_external_fetch_hook(fetch_hook)

      self$storage_driver <- storage_driver
      self$fetch_hook <- fetch_hook

      public <- ls(storage_driver)
      funs <- vlapply(public, function(i) is.function(storage_driver[[i]]))
      funs <- setdiff(names(funs[funs]), ls(self))
      make_method <- function(name) {
        force(name)
        function(...) storage_driver[[name]](...)
      }
      for (f in funs) {
        self[[f]] <- make_method(f)
        lockBinding(substitute(f), self)
      }
      ## Lock up after ourselves so that things are immutable:
      lockEnvironment(self)
    },

    get_hash=function(key, namespace) {
      if (self$storage_driver$exists_key(key, namespace)) {
        self$storage_driver$get_hash(key, namespace)
      } else {
        catch_key_error <- function(e) {
          stop(KeyErrorExternal(key, e, self))
        }
        value <- tryCatch(self$fetch_hook(key, namespace),
                          error=catch_key_error)
        ## Then we have to set things up; this is copied from
        ## storr::set() and from storr:set_value().  This would be
        ## heaps easier if we could access the underlying storr but
        ## that's OK.  This *will* result in a cache miss
        ## unfortunately but the driver does not know about that.
        ##
        ## This does make me wonder if I'm implementing this at the
        ## wrong level (e.g., the external lookup goes in storr).  The
        ## difficulty there is that we have to generate user-friendly
        ## interfaces rather than the "..." interfaces used above.
        hash <- hash_object(value)
        if (!self$storage_driver$exists_hash(hash)) {
          self$storage_driver$set_hash_value(hash, value)
        }
        self$storage_driver$set_key_hash(key, hash, namespace)
        ## We should be fine returning `hash` here, but this acts as a
        ## check that reading from the underlying driver is fine as
        ## subsequent calls will route through the driver.
        self$storage_driver$get_hash(key, namespace)
      }
    }))

check_external_fetch_hook <- function(fetch_hook) {
  assert_function(fetch_hook)
  if (!identical(names(formals(fetch_hook)), c("key", "namespace"))) {
    stop("Function arguments must be 'key', 'namespace'")
  }
}

##' Hook to fetch a resource from a file, for use with driver_external
##' @title Hook to fetch a resource from a file.
##' @param fpath Function to convert \code{key, namespace} into a file path
##' @param fread Function for converting \code{filename} into an R pobject
##' @export
##' @examples
##' hook <- fetch_hook_read(
##'     function(key, namespace) paste0(key, ".csv"),
##'     function(filename) read.csv(filename, stringsAsFactors=FALSE))
fetch_hook_read <- function(fpath, fread) {
  assert_function(fread)
  function(key, namespace) {
    fread(fpath(key, namespace))
  }
}
