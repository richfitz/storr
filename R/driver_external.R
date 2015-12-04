##' storr for fetching external resources.  This does not do a full
##' cascade (that will be implemented elsewhere) but does a very
##' simple pattern where if a key cannot be found in the storr we go
##' out to some external source to find it.
##' @title Storr that kooks for external resources
##' @param storage_driver Another \code{storr} driver to handle the
##'   actual storage.
##' @param fetch_hook A function to run to fetch data when a key is
##'   not found in the store.  This function must throw an error (of
##'   any type) if the external resource cannot be resolved.
##' @param default_namespace Default namespace (see
##'   \code{\link{storr}})
##' @param mangle_key Mangle key? (see \code{\link{storr}})
##' @export
storr_external <- function(storage_driver, fetch_hook,
                           default_namespace="objects", mangle_key=FALSE) {
  .R6_storr_external(storage_driver, fetch_hook,
                     default_namespace, mangle_key)$new()
}

.R6_storr_external <- function(storage_driver, fetch_hook,
                               default_namespace, mangle_key) {
  ## NOTE: This uses inheritence.  I actually think that this might be
  ## the right call here.  This could be implemented as a has-a
  ## relationship but we fundamentally want to interact with this as
  ## an is-a.  Unlike the previous implementation of external storr
  ## objects this one does not suffer the cache miss which is nice.
  ##
  ## At the same time I think there would be a nice symmetry if we did
  ## this with composition so that the initializer takes a storr
  ## object and replaces methods.  That would allow for arbitrarily
  ## deep nesting at the cost of some not-very-standard R6 code.
  ##
  ## NOTE: A downside of implementing this as a storr not a driver is
  ## that if the TTL support turns up in the driver it won't necessarily
  ## carry across.
  super <- self <- NULL # resolved later
  st <- .R6_storr(storage_driver, default_namespace, mangle_key)
  check_external_fetch_hook(fetch_hook)
  R6::R6Class(
    "storr_external",
    inherit=st,
    public=list(
      get_hash=function(key, namespace, use_cache=TRUE) {
        if (!self$exists(key, namespace)) {
          value <- tryCatch(fetch_hook(key, namespace),
                            error=function(e)
                              stop(KeyErrorExternal(key, namespace, e)))
          hash <- self$set(key, value, namespace, use_cache)
          hash
        } else {
          super$get_hash(key, namespace, use_cache)
        }
      }
    ))
}

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
