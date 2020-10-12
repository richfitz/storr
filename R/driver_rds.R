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
##' @section Corrupt keys:
##'
##' Some file synchronisation utilities like dropbox can create file
##' that confuse an rds storr (e.g.,
##' \code{"myobject (Someone's conflicted copy)"}.  If
##' \code{mangle_key} is \code{FALSE} these cannot be detected but at
##' the same time are not a real problem for storr.  However, if
##' \code{mangle_key} is \code{TRUE} and keys are base64 encoded then
##' these conflicted copies can break parts of storr.
##'
##' If you see a warning asking you to deal with these files, please
##' delete the offending files; the path will be printed along with
##' the files that are causing the problem.
##'
##' Alternatively, you can try (assuming a storr object \code{st})
##' running
##'
##' \preformatted{
##' st$driver$purge_corrupt_keys()
##' }
##'
##' which will delete corrupted keys with no confirmation.  The
##' messages that are printed to screen will be printed by default at
##' most once per minute per namespace.  You can control this by
##' setting the R option \code{storr.corrupt.notice.period} - setting
##' this to \code{NA} suppresses the notice and otherwise it is
##' interpreted as the number of seconds.
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
    path_scratch = NULL,
    compress = NULL,
    mangle_key = NULL,
    mangle_key_pad = NULL,
    hash_algorithm = NULL,
    hash_length = NULL,
    traits = list(accept = "raw", throw_missing = TRUE),

    initialize = function(path, compress, mangle_key, mangle_key_pad,
                          hash_algorithm) {
      is_new <- !file.exists(file.path(path, "config"))
      dir_create(path)
      dir_create(file.path(path, "data"))
      dir_create(file.path(path, "keys"))
      dir_create(file.path(path, "config"))
      self$path <- normalizePath(path, mustWork = TRUE)

      self$path_scratch <- file.path(self$path, "scratch")
      dir_create(self$path_scratch)

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
      write_if_missing(as.character(utils::packageVersion("storr")),
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
      
      self$hash_length <- nchar(
        digest::digest(as.raw(0x00), self$hash_algorithm, serialize = FALSE))
    },

    type = function() {
      "rds"
    },

    destroy = function() {
      unlink(self$path, recursive = TRUE)
    },

    get_hash = function(key, namespace) {
      read_text_file(self$name_key(key, namespace), self$hash_length)
    },

    set_hash = function(key, namespace, hash) {
      dir_create(self$name_key("", namespace))
      write_lines(hash, self$name_key(key, namespace),
                  scratch_dir = self$path_scratch)
    },

    get_object = function(hash) {
      read_rds(self$name_hash(hash))
    },

    set_object = function(hash, value) {
      ## NOTE: this takes advantage of having the serialized value
      ## already and avoids seralising twice.
      assert_raw(value)
      write_serialized_rds(value, self$name_hash(hash), self$compress,
                           self$path_scratch)
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
      path <- file.path(self$path, "keys", namespace)
      files <- dir(path)
      if (self$mangle_key) {
        ret <- decode64(files, error = FALSE)
        if (anyNA(ret)) {
          message_corrupted_rds_keys(namespace, path, files[is.na(ret)])
          ret <- ret[!is.na(ret)]
        }
      } else {
        ret <- files
      }
      ret
    },

    check_objects = function(full, hash_length, progress) {
      check_rds_objects(self, full, hash_length, progress)
    },

    check_keys = function(full, hash_length, progress, invalid_hashes) {
      check_rds_keys(self, full, hash_length, progress, invalid_hashes)
    },

    purge_corrupt_keys = function(namespace) {
      if (self$mangle_key) {
        path <- file.path(self$path, "keys", namespace)
        files <- dir(path)
        i <- is.na(decode64(files, error = FALSE))
        if (any(i)) {
          res <- file.remove(file.path(path, files[i]))
          message(sprintf("Removed %d of %d corrupt %s",
                          sum(res), sum(i), ngettext(sum(i), "file", "files")))
        }
      }
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

check_rds_keys <- function(dr, full, hash_length, progress, invalid_hashes) {
  ns <- dr$list_namespaces()
  ret <- lapply(ns, check_rds_keys1, dr, full, hash_length, invalid_hashes)

  collect <- function(k) {
    dat <- lapply(ret, "[[", k)
    cbind(namespace = rep(ns, lengths(dat)), key = unlist(dat, FALSE, FALSE))
  }

  list(corrupt = collect("corrupt"), dangling = collect("dangling"))
}


check_rds_keys1 <- function(ns, dr, full, hash_length, invalid_hashes) {
  keys <- dr$list_keys(ns)
  files <- dr$name_key(keys, ns)

  if (full) {
    hashes <- setdiff(dr$list_hashes(), invalid_hashes)
    d <- lapply(files, readLines)

    re <- sprintf("^[[:xdigit:]]{%d}$", hash_length)
    corrupt <- !vlapply(d, function(x) length(x) == 1L && grepl(re, x))

    dangling <- !corrupt
    dangling[dangling] <- !vlapply(d[dangling], `%in%`, hashes)
  } else {
    len <- file_size(files)
    ## Allow for these to have been written with no newline, unix
    ## newline or windows crlf.
    corrupt <- len < hash_length | len > hash_length + 2L
    dangling <- logical()
  }

  list(corrupt = keys[corrupt],
       dangling = keys[dangling])
}


check_rds_objects <- function(dr, full, hash_length, progress) {
  h <- dr$list_hashes()
  files <- dr$name_hash(h)

  if (full) {
    errs <- 0L
    note_error <- function(e) {
      errs <<- errs + 1L
      TRUE
    }
    check <- function(x) {
      tryCatch({
        suppressWarnings(readRDS(x))
        FALSE
      }, error = note_error)
    }
    n <- length(files)
    if (progress && n > 0 && requireNamespace("progress", quietly = TRUE)) {
      tick <- progress::progress_bar$new(
        format = "[:spin] [:bar] :percent (:errs corrupt)",
        total = n)$tick
    } else {
      tick <- function(...) NULL
    }
    err <- logical(length(files))
    for (i in seq_along(files)) {
      err[i] <- check(files[[i]])
      tick(tokens = list(errs = errs))
    }
  } else {
    err <- file_size(files) == 0L
  }

  list(corrupt = h[err])
}


corrupt_notices <- new.env(parent = emptyenv())


message_corrupted_rds_keys <- function(namespace, path, files) {
  period <- getOption("storr.corrupt.notice.period", 60)
  if (is.na(period)) {
    return()
  }
  last <- corrupt_notices[[path]]
  now <- Sys.time()
  if (!is.null(last)) {
    if (as.numeric(now - last, "secs") < period) {
      return()
    }
  }

  "%d corrupted files have been found in your storr archive:

namespace: '%s'
path: '%s'
files:
%s

See 'Corrupt keys' within ?storr_rds for how to proceed" -> fmt
  files <- sprintf("  - '%s'", files)
  message(sprintf(fmt, length(files), namespace, path, files))
  corrupt_notices[[path]] <- now
}
