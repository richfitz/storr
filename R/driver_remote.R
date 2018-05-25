##' Create a storr that keeps rds-serialised objects on a remote
##' location.  This is the abstract interface (which does not do
##' anything useful) but which can be used with file operation driver
##' to store files elsewhere.  This is not intended for end-user use
##' so there is no \code{storr_remote} function.  Instead this
##' function is designed to support external packages that implement
##' the details.  For a worked example, see the package tests
##' (\code{helper-remote.R}).  In the current implementation these
##' build off of the \code{\link{driver_rds}} driver by copying files
##' to some remote location.
##'
##' @title Remote storr
##'
##' @param ops A file operations object.  See tests for now to see
##'   what is required to implement one.
##'
##' @param ... Arguments to pass through to \code{\link{driver_rds}},
##'   including \code{compress}, \code{mangle_key},
##'   \code{mangle_key_pad} and \code{hash_algorithm}.
##'
##' @param path_local Path to a local cache.  This can be left as
##'   \code{NULL}, in which case a per-session cache will be used.
##'   Alternatively, explicitly set to a path and the cache can be
##'   reused over sessions.  Only storr \emph{values} (i.e., objects)
##'   are cached - the key-to-value mapping is always fetched from the
##'   remote storage.
##'
##' @export
##' @author Rich FitzJohn
driver_remote <- function(ops, ..., path_local = NULL) {
  R6_driver_remote$new(ops, ..., path_local = path_local)
}


R6_driver_remote <- R6::R6Class(
  "driver_remote",

  public = list(
    ops = NULL,
    rds = NULL,
    traits = NULL,
    hash_algorithm = NULL,

    initialize = function(ops, ..., path_local) {
      ## For the configuration, we should pass along all dots to the
      ## remote storr and check?  Or do that when controlling the
      ## local rds.  So this is going to get a lot of dots and do the
      ## build itself.
      self$ops <- ops

      ## TODO: there's not a great deal of use using a local cache if
      ## passed NULL because we already have access to the storr's
      ## environment cache.
      path_local <- path_local %||% tempfile()

      config <- storr_remote_config_get(self$ops)
      extra <- storr_remote_config_validate(config, path_local, ...)
      storr_remote_config_set(self$ops, extra)
      ## TODO: throughput, use of "config", "keys" and "data" use
      ## storr_rds internal details.
      self$ops$create_dir("data")
      self$ops$create_dir("keys")

      self$rds <- driver_rds(path_local, ...)
      self$traits <- storr_traits(self$rds$traits)
      self$hash_algorithm <- self$rds$hash_algorithm
    },

    type = function() {
      sprintf("remote/%s", self$ops$type())
    },

    destroy = function() {
      self$ops$destroy()
      self$ops <- NULL
    },

    get_hash = function(key, namespace) {
      self$ops$read_string(self$name_key(key, namespace))
    },

    set_hash = function(key, namespace, hash) {
      self$ops$write_string(hash, self$name_key(key, namespace))
    },

    get_object = function(hash) {
      if (!self$rds$exists_object(hash)) {
        filename_hash <- self$name_hash(hash)
        dest_dir <- dirname(self$rds$name_hash(hash))
        self$ops$download_file(filename_hash, dest_dir)
      }
      self$rds$get_object(hash)
    },

    set_object = function(hash, value) {
      filename_remote <- self$name_hash(hash)
      if (!self$ops$exists(filename_remote)) {
        filename_local <- self$rds$name_hash(hash)
        if (!file.exists(filename_local)) {
          self$rds$set_object(hash, value)
        }
        self$ops$upload_file(filename_local, dirname(filename_remote))
      }
    },

    exists_hash = function(key, namespace) {
      self$ops$exists(self$name_key(key, namespace))
    },

    exists_object = function(hash) {
      self$ops$exists(self$name_hash(hash))
    },


    del_hash = function(key, namespace) {
      self$ops$delete_file(self$name_key(key, namespace))
    },

    del_object = function(hash) {
      self$ops$delete_file(self$name_hash(hash))
    },

    list_hashes = function() {
      sub("\\.rds$", "", self$ops$list_dir("data"))
    },

    list_namespaces = function() {
      self$ops$list_dir("keys")
    },

    list_keys = function(namespace) {
      path <- file.path("keys", namespace)
      if (!self$ops$exists_dir(path)) {
        return(character(0))
      }
      ret <- self$ops$list_dir(path)
      if (self$rds$mangle_key) decode64(ret, TRUE) else ret
    },

    ## These functions could be done better if driver_rds takes a
    ## 'relative' argument
    name_hash = function(hash) {
      p <- self$rds$name_hash(hash)
      file.path(basename(dirname(p)), basename(p))
    },

    name_key = function(key, namespace) {
      p <- self$rds$name_key(key, namespace)
      file.path(
        basename(dirname(dirname(p))),
        basename(dirname(p)),
        basename(p))
    }))


## It would be really nice to do this as a single operation but that
## probably can't be easily done generally.  Quite possibly it would
## be possible to get/fetch an entire directory though.
##
## The other option is using a single remote object, which loses the
## ability to have a remote storr really reflect a local one...
storr_remote_config_get <- function(ops) {
  ## NOTE: this is a storr/rds internal
  path_config <- "config"
  if (ops$exists_dir(path_config)) {
    keys <- ops$list_dir(path_config)
    ret <- lapply(file.path(path_config, keys), ops$read_string)
    names(ret) <- keys
  } else {
    ret <- NULL
  }
  ret
}


storr_remote_config_set <- function(ops, data) {
  path_config <- "config"
  for (key in names(data)) {
    ops$write_string(data[[key]], file.path(path_config, key))
  }
}


storr_remote_config_validate <- function(prev, path_local, ...) {
  path_config <- "config"

  ## This exploits quite a bit of storr's internals:
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  path_config_tmp <- file.path(tmp, path_config)

  ## The storr rds driver requires that the config directory not exist
  ## at all
  if (!is.null(prev)) {
    dir.create(path_config_tmp, FALSE, TRUE)
    for (key in names(prev)) {
      writeLines(prev[[key]], file.path(path_config_tmp, key))
    }
  }

  ## This will error if the options can't be supported, but we never
  ## use the driver itself for anything.
  dr <- driver_rds(tmp, ...)

  ## These are the configuration elements to set remotely:
  extra <- setdiff(dir(path_config_tmp), names(prev))
  if (length(extra) > 0L) {
    ret <- lapply(file.path(path_config_tmp, extra), readLines)
    names(ret) <- extra
  } else {
    ret <- NULL
  }

  ## Replicate our temporary configuration into the local cache:
  path_config_local <- file.path(path_local, path_config)
  msg <- setdiff(dir(path_config_tmp), dir(path_config_local))
  dir.create(path_config_local, FALSE, TRUE)
  file.copy(file.path(path_config_tmp, msg), path_config_local)

  ret
}
