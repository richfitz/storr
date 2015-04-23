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
      dir.create(path, FALSE, TRUE)
      dir.create(self$path_data, FALSE, TRUE)
      dir.create(self$path_keys, FALSE, TRUE)
    },

    exists_hash=function(hash) {
      file.exists(self$name_data(hash))
    },
    exists_key=function(key) {
      file.exists(self$name_key(key))
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      saveRDS(value, self$name_data(hash), compress=self$compress)
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash) {
      writeLines(hash, self$name_key(key))
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
    get_hash=function(key) {
      name <- self$name_key(key)
      if (file.exists(name)) {
        readLines(name)
      } else {
        stop(KeyError(key))
      }
    },

    del_hash=function(hash) {
      suppressWarnings(file.remove(self$name_data(hash)))
    },
    del_key=function(key) {
      suppressWarnings(file.remove(self$name_key(key)))
    },

    list_hashes=function() {
      sub("\\.rds$", "", dir(self$path_data))
    },
    list_keys=function() {
      dir(self$path_keys)
    },

    name_data=function(hash) {
      sprintf("%s/%s.rds", self$path_data, hash)
    },
    name_key=function(key) {
      sprintf("%s/%s", self$path_keys, key)
    }
  ))
