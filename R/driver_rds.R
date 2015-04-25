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
    path_list=NULL,
    compress=NULL,

    initialize=function(path, compress) {
      self$path_data <- file.path(path, "data")
      self$path_keys <- file.path(path, "keys")
      self$path_list <- file.path(path, "list")
      self$compress <- compress
      dir_create(path)
      dir_create(self$path_data)
      dir_create(self$path_keys)
      dir_create(self$path_list)
    },

    exists_hash=function(hash) {
      file.exists(self$name_data(hash))
    },
    exists_key=function(key, namespace) {
      file.exists(self$name_key(key, namespace))
    },

    ## Write some data into its hash value
    set_hash_value=function(hash, value) {
      saveRDS(value, self$name_data(hash), compress=self$compress)
    },
    ## Associate a key with some data
    set_key_hash=function(key, hash, namespace) {
      dir_create(self$name_key("", namespace))
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
    get_hash=function(key, namespace) {
      name <- self$name_key(key, namespace)
      if (file.exists(name)) {
        readLines(name)
      } else {
        stop(KeyError(key))
      }
    },

    del_hash=function(hash) {
      file_remove(self$name_data(hash))
    },
    del_key=function(key, namespace) {
      file_remove(self$name_key(key, namespace))
    },

    list_hashes=function() {
      sub("\\.rds$", "", dir(self$path_data))
    },
    list_keys=function(namespace) {
      dir(self$name_key("", namespace),
          all.files=TRUE, no..=TRUE)
    },

    ## List support:
    is_list=function(key, namespace) {
      file.exists(self$name_list(key, namespace))
    },
    length_list=function(key, namespace) {
      name <- self$name_list(key, namespace)
      if (file.exists(name)) {
        count_lines(name)
      } else {
        0L
      }
    },

    set_key_hash_list=function(key, i, hash, namespace) {
      dir_create(self$name_list("", namespace))
      name <- self$name_list(key, namespace)
      if (is.null(i)) {
        writeLines(hash, name)
      } else if (self$is_list(key, namespace)) {
        list_check_range(key, i, self$length_list(key, namespace))
        tmp <- readLines(name)
        tmp[i] <- hash
        writeLines(tmp, name)
      } else {
        ## TODO: driver-level type information - none or string here?
        stop(TypeError(key, "list", "other"))
      }
    },

    get_hash_list=function(key, i, namespace) {
      name <- self$name_list(key, namespace)
      if (file.exists(name)) {
        hash <- readLines(name)
      } else {
        hash <- character(0)
      }
      if (is.null(i)) {
        hash
      } else {
        list_check_range(key, i, length(hash))
        hash[i]
      }
    },
    del_hash_list=function(key, namespace) {
      file_remove(self$name_list(key, namespace))
    },

    name_data=function(hash) {
      sprintf("%s/%s.rds", self$path_data, hash)
    },
    name_key=function(key, namespace) {
      sprintf("%s/%s/%s", self$path_keys, namespace, key)
    },
    name_list=function(key, namespace) {
      sprintf("%s/%s/%s", self$path_list, namespace, key)
    }
  ))
