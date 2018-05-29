##' Create a special storr that uses separate storage drivers for the
##' keys (which tend to be numerous and small in size) and the data
##' (which tends to be somewhat less numerous and much larger in
##' size).  This might be useful to use storage models with different
##' characteristics (in memory/on disk, etc).
##'
##' This is an experimental feature and somewhat subject to change.
##' In particular, the driver may develop the ability to store small
##' data in the same storr as the keys (say, up to 1kb) based on some
##' tunable parameter.
##'
##' You can attach another storr to either the data or the key storage
##' (see the example), but it will not be able to see keys or data
##' (respectively).  If you garbage collect the data half, all the
##' data will be lost!
##'
##' @title Storr with multiple storage drivers
##' @param keys Driver for the keys
##' @param data Driver for the data
##' @param default_namespace Default namespace (see
##'   \code{\link{storr}}).
##' @export
##' @examples
##' # Create a storr that is stores keys in an environment and data in
##' # an rds
##' path <- tempfile()
##' st <- storr::storr_multistorr(driver_environment(),
##'                               driver_rds(path))
##' st$set("a", runif(10))
##' st$get("a")
##'
##' # The data can be also seen by connecting to the rds store
##' rds <- storr::storr_rds(path)
##' rds$list() # empty
##' rds$list_hashes() # here's the data
##' rds$get_value(rds$list_hashes())
##'
##' st$destroy()
storr_multistorr <- function(keys, data, default_namespace = "objects") {
  storr(driver_multistorr(keys, data), default_namespace)
}


driver_multistorr <- function(keys, data) {
  R6_driver_multistorr$new(keys, data)
}


R6_driver_multistorr <- R6::R6Class(
  "driver_multistorr",
  cloneable = FALSE,
  public = list(
    keys = NULL,
    data = NULL,
    traits = NULL,
    hash_algorithm = NULL,

    ## pseudo-methods
    get_hash = NULL,
    set_hash = NULL,
    get_object = NULL,
    set_object = NULL,
    exists_hash = NULL,
    exists_object = NULL,
    del_hash = NULL,
    del_object = NULL,
    list_hashes = NULL,
    list_keys = NULL,
    list_namespaces = NULL,

    ## real methods
    initialize = function(keys, data) {
      self$keys <- assert_probably_storr_driver(keys)
      self$data <- assert_probably_storr_driver(data)

      self$traits <- storr_traits(self$data$traits)
      self$traits$throw_missing <- self$traits$throw_missing &&
        storr_traits(self$keys$traits)$throw_missing
      self$hash_algorithm <- data$hash_algorithm

      self$get_hash <- self$keys$get_hash
      self$set_hash <- self$keys$set_hash
      self$exists_hash <- self$keys$exists_hash
      self$del_hash <- self$keys$del_hash
      self$list_keys <- self$keys$list_keys
      self$list_namespaces <- self$keys$list_namespaces

      self$get_object <- self$data$get_object
      self$set_object <- self$data$set_object
      self$exists_object <- self$data$exists_object
      self$del_object <- self$data$del_object
      self$list_hashes <- self$data$list_hashes
    },

    type = function() {
      sprintf("multistorr (keys: %s, data: %s)",
              self$keys$type(), self$data$type())
    },

    destroy = function() {
      self$keys$destroy()
      self$data$destroy()
    }))
