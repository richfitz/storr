##' RDS object cache driver
##'
##' The \code{mangle_key} argument will run each key that is created
##' through a "base 64" encoding.  This means that keys that include
##' symbols that are invalid on filesystems (e.g, "/", ":") will be
##' replaced by harmless characters.  The RFC 4648 dialect is used
##' where "-" and "_" are used for character 62 and 63 (this differs
##' from most R base64 encoders).  This mangling is designed to be
##' transparent to the user -- the storr will appear to store things
##' with unmangled keys but the names of the stored files will be
##' different.
##'
##' Note that the \emph{namespace} is not mangled (at least not yet)
##' so needs to be a valid filename.
##'
##' Because the actual file will be stored with mangled names it is
##' not safe to use the same path for a storr with and without
##' mangling.  So once an rds storr has been created its "mangledness"
##' is set.  Using \code{mangle_key=NULL} uses whatever mangledness
##' exists (or no mangledness if creating a new storr).
##'
##' @title RDS object cache driver
##' @param path Path for the store.  \code{tempdir()} is a good choice
##'   for ephemeral storage, \code{rappdirs} might be nice for
##'   application data.
##' @param compress Compress the generated file?  This saves a small
##'   amount of space for a reasonable amount of time.
##' @param mangle_key Mangle keys?  If TRUE, then the key is encoded
##'   using base64 before saving to the filesystem.  See Details.
##' @export
driver_rds <- function(path, compress=TRUE, mangle_key=NULL) {
  .R6_driver_rds$new(path, compress, mangle_key)
}

##' @export
##' @rdname driver_rds
##' @param default_namespace Default namespace (see \code{\link{storr}})
storr_rds <- function(path, compress=TRUE, mangle_key=NULL,
                      default_namespace="objects") {
  storr(driver_rds(path, compress, mangle_key), default_namespace)
}

.R6_driver_rds <- R6::R6Class(
  "driver_rds",
  public=list(
    path=NULL,
    compress=NULL,
    mangle_key=NULL,
    initialize=function(path, compress, mangle_key) {
      dir_create(path)
      dir_create(file.path(path, "data"))
      dir_create(file.path(path, "keys"))
      dir_create(file.path(path, "config"))
      self$path <- path
      self$compress <- compress

      ## This attempts to check that we are connecting to a storr of
      ## appropriate mangledness.  There's a lot of logic here, but
      ## it's actually pretty simple in practice and tested in
      ## test-driver-rds.R:
      ##
      ##   if mangle_key is NULL we take the mangledless of the
      ##   existing storr or set up for no mangling.
      ##
      ##   if mangle_key is not NULL then it is an error if it differs
      ##   from the existing storr's mangledness.
      if (!is.null(mangle_key)) {
        assert_scalar_logical(mangle_key)
      }
      path_mangled <- file.path(path, "config", "mangle_key")
      if (file.exists(path_mangled)) {
        mangle_key_prev <- as.logical(readLines(path_mangled))
        if (is.null(mangle_key)) {
          mangle_key <- mangle_key_prev
        } else if (mangle_key != mangle_key_prev) {
          stop(sprintf("Incompatible mangledness (existing: %s, requested: %s)",
                       mangle_key_prev, mangle_key))
        }
      } else {
        if (is.null(mangle_key)) {
          mangle_key <- FALSE
        }
        writeLines(as.character(mangle_key), path_mangled)
      }
      self$mangle_key <- mangle_key
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
    list_namespaces=function() {
      dir(file.path(self$path, "keys"))
    },
    list_keys=function(namespace) {
      ret <- dir(file.path(self$path, "keys", namespace))
      if (self$mangle_key) decode64(ret, TRUE) else ret
    },

    name_hash=function(hash) {
      file.path(self$path, "data", paste0(hash, ".rds"))
    },
    name_key=function(key, namespace) {
      if (self$mangle_key) {
        key <- encode64(key)
      }
      file.path(self$path, "keys", namespace, key)
    }
  ))
