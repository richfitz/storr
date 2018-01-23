storr_multistorr <- function(keys, data, default_namespace = "objects") {
  storr::storr(driver_multistorr(keys, data), default_namespace)
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
      self$keys <- keys
      self$data <- data

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
