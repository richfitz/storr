##' RDS object cache driver
##' @title RDS object cache driver
##' @param path Path for the store.  \code{tempdir()} is a good choice
##' for ephemeral storage, \code{rappdirs} might be nice for
##' application data.
##' @param compress Compress the generated file?  This saves a small
##' amount of space for a reasonable amount of time.
##' @export
driver_rds <- function(path, compress=FALSE) {
  .R6_driver_rds$new(path, compress)
}

.R6_driver_rds <- R6::R6Class(
  "driver_rds",

  public=list(
    path_data=NULL,
    path_keys=NULL,
    compress=NULL,

    initialize=function(path, compress) {
      self$path_data <- file.path(path, "data")
      self$path_keys <- file.path(path, "keys")
      self$compress <- compress
      dir_create(path)
      dir_create(self$path_data)
      dir_create(self$name_key("", namespace="objects"))
    },

    exists_hash=function(hash) {
      file.exists(self$name_data(hash))
    },
    exists_key=function(key, namespace="objects") {
      file.exists(self$name_key(key, namespace))
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      saveRDS(value, self$name_data(hash), compress=self$compress)
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash, namespace="objects") {
      if (namespace != "objects") {
        dir_create(self$name_key("", namespace))
      }
      writeLines(hash, self$name_key(key, namespace))
    },

    ## Get value, given hash
    get_value=function(hash) {
      name <- self$name_data(hash)
      if (file.exists(name)) {
        readRDS(name)
      } else {
        stop(HashError(hash))
      }
    },
    ## Get hash, given key
    get_hash=function(key, namespace="objects") {
      name <- self$name_key(key, namespace)
      if (file.exists(name)) {
        readLines(name)
      } else {
        stop(KeyError(key))
      }
    },

    del_hash=function(hash) {
      suppressWarnings(file.remove(self$name_data(hash)))
    },
    del_key=function(key, namespace="objects") {
      suppressWarnings(file.remove(self$name_key(key, namespace)))
    },

    list_hashes=function() {
      sub("\\.rds$", "", dir(self$path_data))
    },
    list_keys=function(namespace="objects") {
      dir(self$name_key("", namespace),
          all.files=TRUE, no..=TRUE)
    },

    name_data=function(hash) {
      sprintf("%s/%s.rds", self$path_data, hash)
    },
    name_key=function(key, namespace="objects") {
      sprintf("%s/%s/%s", self$path_keys, namespace, key)
    }
  ))
