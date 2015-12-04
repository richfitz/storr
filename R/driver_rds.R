##' RDS object cache driver
##' @title RDS object cache driver
##' @param path Path for the store.  \code{tempdir()} is a good choice
##' for ephemeral storage, \code{rappdirs} might be nice for
##' application data.
##' @param compress Compress the generated file?  This saves a small
##' amount of space for a reasonable amount of time.
##' @export
driver_rds <- function(path, compress=TRUE) {
  .R6_driver_rds$new(path, compress)
}

##' @export
##' @rdname driver_rds
##' @param default_namespace Default namespace (see \code{\link{storr}})
##' @param mangle_key Mangle key? (see \code{\link{storr}})
storr_rds <- function(path, compress=TRUE,
                      default_namespace="objects", mangle_key=FALSE) {
  storr(driver_rds(path, compress), default_namespace, mangle_key)
}

.R6_driver_rds <- R6::R6Class(
  "driver_rds",
  public=list(
    path=NULL,
    compress=NULL,
    initialize=function(path, compress) {
      dir_create(path)
      dir_create(file.path(path, "data"))
      dir_create(file.path(path, "keys"))
      self$path <- path
      self$compress <- compress
    },

    type=function() {
      "rds"
    },
    destroy=function() {
      unlink(self$path, recursive=TRUE)
    },

    get_hash=function(key, namespace) {
      readLines(self$name_key(key, namespace))
    },
    set_hash=function(key, namespace, hash) {
      dir_create(self$name_key("", namespace))
      writeLines(hash, self$name_key(key, namespace))
    },
    get_object=function(hash) {
      readRDS(self$name_hash(hash))
    },
    set_object=function(hash, value) {
      saveRDS(value, self$name_hash(hash), compress=self$compress)
    },

    exists_key=function(key, namespace) {
      file.exists(self$name_key(key, namespace))
    },
    exists_hash=function(hash) {
      file.exists(self$name_hash(hash))
    },

    del_key=function(key, namespace) {
      file_remove(self$name_key(key, namespace))
    },
    del_hash=function(hash) {
      file_remove(self$name_hash(hash))
    },

    list_hashes=function() {
      sub("\\.rds$", "", dir(file.path(self$path, "data")))
    },
    list_keys=function(namespace) {
      dir(file.path(self$path, "keys", namespace))
    },
    list_namespaces=function() {
      dir(file.path(self$path, "keys"))
    },

    name_hash=function(hash) {
      file.path(self$path, "data", paste0(hash, ".rds"))
    },
    name_key=function(key, namespace) {
      file.path(self$path, "keys", namespace, key)
    }
  ))
