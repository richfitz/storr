##' storr for fetching external resources.  This driver is used where
##' will try to fetch from an external data source if a resource can
##' not be found locally.  This works by checking to see if a key is
##' present in the storr (and if so returning it).  If it is not
##' found, then the function `fetch_hook` is run to fetch it.
##'
##' See the vignette `vignette("external")` for much more detail.
##' This function is likely most useful for things like caching
##' resources from websites, or computing long-running quantities on
##' demand.
##' @title Storr that looks for external resources
##' @param storage_driver Another `storr` driver to handle the
##'   actual storage.
##' @param fetch_hook A function to run to fetch data when a key is
##'   not found in the store.  This function must take arguments
##'   `key` and `namespace` and return an R object.  It must
##'   throw an error if the external resource cannot be resolved.
##' @param default_namespace Default namespace (see
##'   [storr])
##' @export
storr_external <- function(storage_driver, fetch_hook,
                           default_namespace = "objects") {
  R6_storr_external$new(storage_driver, fetch_hook, default_namespace)
}


## NOTE: This uses inheritence.  I actually think that this might be
## the right call here.  This could be implemented as a has-a
## relationship but we fundamentally want to interact with this as an
## is-a.  Unlike the previous implementation of external storr objects
## this one does not suffer the cache miss which is nice.
##
## TODO: Need "pending" support here for parallel invocations?  Not
## possible through most backends?
##
## TODO: Support passing in an actual storr here, in which case we'd
## just take the driver element from it (possibly with a clone).
R6_storr_external <- R6::R6Class(
  "storr_external",
  inherit = R6_storr,
  public = list(
    fetch_hook = NULL,
    initialize = function(storage_driver, fetch_hook, default_namespace) {
      super$initialize(storage_driver, default_namespace)
      self$fetch_hook <- check_external_fetch_hook(fetch_hook)
    },
    ## NOTE: This is *always* using use_cache = TRUE in the set phase.
    ## I think that's OK because it doesn't make a great deal of sense
    ## to expose use_cache in the get_hash function which generally
    ## will not touch the cache.
    get_hash = function(key, namespace) {
      if (!self$exists(key, namespace)) {
        value <- tryCatch(self$fetch_hook(key, namespace),
                          error = function(e)
                            stop(KeyErrorExternal(key, namespace, e)))
        hash <- self$set(key, value, namespace)
        hash
      } else {
        super$get_hash(key, namespace)
      }
    }))


check_external_fetch_hook <- function(fetch_hook) {
  ## NOTE: Could check here that there are two arguments, and that the
  ## argument names are (key, namespace) but that seems overly
  ## restrictive and hard to get right
  assert_function(fetch_hook)
  fetch_hook
}

##' Hook to fetch a resource from a file, for use with
##' driver_external.  We take two functions as arguments: the first
##' converts a key/namespace pair into a filename, and the second
##' reads from that filename.  Because many R functions support
##' reading from URLs `fetch_hook_read` can be used to read from
##' remote resources.
##'
##' For more information about using this, see
##' [storr_external] (this can be used as a
##' `fetch_hook` argument) and the vignette:
##' `vignette("external")`
##' @title Hook to fetch a resource from a file.
##' @param fpath Function to convert `key, namespace` into a file
##'   path
##' @param fread Function for converting `filename` into an R
##'   object
##' @export
##' @examples
##' hook <- fetch_hook_read(
##'     function(key, namespace) paste0(key, ".csv"),
##'     function(filename) read.csv(filename, stringsAsFactors = FALSE))
fetch_hook_read <- function(fpath, fread) {
  check_external_fetch_hook(fpath)
  assert_function(fread)
  function(key, namespace) {
    fread(fpath(key, namespace))
  }
}
