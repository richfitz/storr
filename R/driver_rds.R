##' Object cache driver that saves objects using R's native
##' serialized file format (see \code{\link{saveRDS}}) on the
##' filesystem.
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
##' so needs to contain characters that are valid in a filename.
##'
##' Because the actual file will be stored with mangled names it is
##' not safe to use the same path for a storr with and without
##' mangling.  So once an rds storr has been created its "mangledness"
##' is set.  Using \code{mangle_key = NULL} uses whatever mangledness
##' exists (or no mangledness if creating a new storr).
##'
##' @title rds object cache driver
##' @param path Path for the store.  \code{tempdir()} is a good choice
##'   for ephemeral storage, The \code{rappdirs} package (on CRAN)
##'   might be nice for persistent application data.
##'
##' @param compress Compress the generated file?  This saves a small
##'   amount of space for a reasonable amount of time.
##'
##' @param mangle_key Mangle keys?  If TRUE, then the key is encoded
##'   using base64 before saving to the filesystem.  See Details.
##'
##' @param mangle_key_pad Logical indicating if the filenames created
##'   when using \code{mangle_key} should also be "padded" with the
##'   \code{=} character to make up a round number of bytes.  Padding
##'   is required to satisfy the document that describes base64
##'   encoding (RFC 4648) but can cause problems in some applications
##'   (see \href{https://github.com/richfitz/storr/issues/43}{this
##'   issue}.  The default is to not pad \emph{new} storr archives.
##'   This should be generally safe to leave alone.
##'
##' @param hash_algorithm Name of the hash algorithm to use.  Possible
##'   values are "md5", "sha1", and others supported by
##'   \code{\link{digest}}.  If not given, then we will default to
##'   "md5".
##'
##' @param default_namespace Default namespace (see
##'   \code{\link{storr}}).
##' @export
##' @examples
##'
##' # Create an rds storr in R's temporary directory:
##' st <- storr_rds(tempfile())
##'
##' # Store some data (10 random numbers against the key "foo")
##' st$set("foo", runif(10))
##' st$list()
##'
##' # And retrieve the data:
##' st$get("foo")
##'
##' # Keys that are not valid filenames will cause issues.  This will
##' # cause an error:
##' \dontrun{
##' st$set("foo/bar", letters)
##' }
##'
##' # The solution to this is to "mangle" the key names.  Storr can do
##' # this for you:
##' st2 <- storr_rds(tempfile(), mangle_key = TRUE)
##' st2$set("foo/bar", letters)
##' st2$list()
##' st2$get("foo/bar")
##'
##' # Behind the scenes, storr is safely encoding the filenames with base64:
##' dir(file.path(st2$driver$path, "keys", "objects"))
##'
##' # Clean up the two storrs:
##' st$destroy()
##' st2$destroy()
storr_rds <- function(path, compress = NULL, mangle_key = NULL,
                      mangle_key_pad = NULL, hash_algorithm = NULL,
                      default_namespace = "objects") {
  storr(driver_rds(path, compress, mangle_key, mangle_key_pad, hash_algorithm),
        default_namespace)
}

##' @export
##' @rdname storr_rds
driver_rds <- function(path, compress = NULL, mangle_key = NULL,
                       mangle_key_pad = NULL, hash_algorithm = NULL) {
  R6_driver_rds$new(path, compress, mangle_key, mangle_key_pad, hash_algorithm)
}

R6_driver_rds <- R6::R6Class(
  "driver_rds",
  public = list(
    ## TODO: things like hash_algorithm: do they belong in traits?
    ## This needs sorting before anyone writes their own driver!
    path = NULL,
    compress = NULL,
    mangle_key = NULL,
    mangle_key_pad = NULL,
    hash_algorithm = NULL,
    traits = list(accept = "raw"),

    initialize = function(path, compress, mangle_key, mangle_key_pad,
                          hash_algorithm) {
      is_new <- !file.exists(file.path(path, "config"))
      dir_create(path)
      dir_create(file.path(path, "data"))
      dir_create(file.path(path, "keys"))
      dir_create(file.path(path, "config"))
      self$path <- path

      ## This is a bit of complicated dancing around to mantain
      ## backward compatibility while allowing better defaults in
      ## future versions.  I'm writing out a version number here that
      ## future versions of driver_rds can use to patch, warn or
      ## change behaviour with older versions of the storr.
      if (!is_new && !file.exists(driver_rds_config_file(path, "version"))) {
        write_if_missing("1.0.1", driver_rds_config_file(path, "version"))
        write_if_missing("TRUE", driver_rds_config_file(path, "mangle_key_pad"))
        write_if_missing("TRUE", driver_rds_config_file(path, "compress"))
        write_if_missing("md5", driver_rds_config_file(path, "hash_algorithm"))
      }
      ## Then write out the version number:
      write_if_missing(as.character(packageVersion("storr")),
                       driver_rds_config_file(path, "version"))

      if (!is.null(mangle_key)) {
        assert_scalar_logical(mangle_key)
      }
      self$mangle_key <- driver_rds_config(path, "mangle_key", mangle_key,
                                           FALSE, TRUE)

      if (!is.null(mangle_key_pad)) {
        assert_scalar_logical(mangle_key_pad)
      }
      self$mangle_key_pad <-
        driver_rds_config(path, "mangle_key_pad", mangle_key_pad,
                          FALSE, TRUE)

      if (!is.null(compress)) {
        assert_scalar_logical(compress)
      }
      self$compress <- driver_rds_config(path, "compress", compress,
                                         TRUE, FALSE)

      if (!is.null(hash_algorithm)) {
        assert_scalar_character(hash_algorithm)
      }
      self$hash_algorithm <- driver_rds_config(path, "hash_algorithm",
                                               hash_algorithm, "md5", TRUE)
    },

    type = function() {
      "rds"
    },
    destroy = function() {
      unlink(self$path, recursive = TRUE)
    },

    get_hash = function(key, namespace) {
      readLines(self$name_key(key, namespace))
    },
    set_hash = function(key, namespace, hash) {
      dir_create(self$name_key("", namespace))
      write_lines(hash, self$name_key(key, namespace))
    },
    get_object = function(hash) {
      readRDS(self$name_hash(hash))
    },
    set_object = function(hash, value) {
      ## NOTE: this takes advantage of having the serialized value
      ## already and avoids seralising twice.
      assert_raw(value)
      write_serialized_rds(value, self$name_hash(hash), self$compress)
    },

    exists_hash = function(key, namespace) {
      file.exists(self$name_key(key, namespace))
    },
    exists_object = function(hash) {
      file.exists(self$name_hash(hash))
    },

    del_hash = function(key, namespace) {
      file_remove(self$name_key(key, namespace))
    },
    del_object = function(hash) {
      file_remove(self$name_hash(hash))
    },

    list_hashes = function() {
      sub("\\.rds$", "", dir(file.path(self$path, "data")))
    },
    list_namespaces = function() {
      dir(file.path(self$path, "keys"))
    },
    list_keys = function(namespace) {
      ret <- dir(file.path(self$path, "keys", namespace))
      if (self$mangle_key) decode64(ret, TRUE) else ret
    },

    name_hash = function(hash) {
      if (length(hash) > 0L) {
        file.path(self$path, "data", paste0(hash, ".rds"))
      } else {
        character(0)
      }
    },
    name_key = function(key, namespace) {
      if (self$mangle_key) {
        key <- encode64(key, pad = self$mangle_key_pad)
      }
      file.path(self$path, "keys", namespace, key)
    }
  ))

## This attempts to check that we are connecting to a storr of
## appropriate mangledness.  There's a lot of logic here, but it's
## actually pretty simple in practice and tested in test-driver-rds.R:
##
##   if mangle_key is NULL we take the mangledless of the
##   existing storr or set up for no mangling.
##
##   if mangle_key is not NULL then it is an error if it differs
##   from the existing storr's mangledness.
driver_rds_config <- function(path, name, value, default, must_agree) {
  path_opt <- driver_rds_config_file(path, name)

  load_value <- function() {
    if (file.exists(path_opt)) {
      value <- readLines(path_opt)
      storage.mode(value) <- storage.mode(default)
    } else {
      value <- default
    }
    value
  }

  if (is.null(value)) {
    value <- load_value()
  } else if (must_agree && file.exists(path_opt)) {
    value_prev <- load_value()
    if (value != value_prev) {
      stop(ConfigError(name, value_prev, value))
    }
  }
  if (!file.exists(path_opt)) {
    writeLines(as.character(value), path_opt)
  }

  value
}

driver_rds_config_file <- function(path, key) {
  file.path(path, "config", key)
}

write_if_missing <- function(value, path) {
  if (!file.exists(path)) {
    writeLines(value, path)
  }
}
