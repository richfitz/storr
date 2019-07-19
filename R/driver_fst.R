##' @include driver_rds.R
##' Object cache driver that saves objects using \code{fst}:
##' \url{https://github.com/fstpackage/fst}.
##' The \code{fst} driver
##' (\code{\link{storr_fst}} and \code{\link{driver_fst}})
##' is modeled directly after the RDS driver
##' (\code{\link{storr_rds}} and \code{\link{driver_rds}}).
##' Most of the features of the rds driver
##' carry over to the \code{fst} driver. For example,
##' key mangling, namespaces, and handling of corrupt keys are the same.
##'
##' @title fst object cache driver
##' @inheritParams storr_rds
##' @export
##' @examples
##'
##' # Create an fst storr in R's temporary directory:
##' st <- storr_fst(tempfile())
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
##' st2 <- storr_fst(tempfile(), mangle_key = TRUE)
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
storr_fst <- function(path, compress = NULL, mangle_key = NULL,
                      mangle_key_pad = NULL, hash_algorithm = NULL,
                      default_namespace = "objects") {
  storr(driver_fst(path, compress, mangle_key, mangle_key_pad, hash_algorithm),
        default_namespace)
}

##' @export
##' @rdname storr_fst
driver_fst <- function(path, compress = NULL, mangle_key = NULL,
                       mangle_key_pad = NULL, hash_algorithm = NULL) {
  R6_driver_fst$new(path, compress, mangle_key, mangle_key_pad, hash_algorithm)
}

R6_driver_fst <- R6::R6Class(
  "driver_fst",
  inherit = R6_driver_rds,
  public = list(

    initialize = function(path, compress, mangle_key, mangle_key_pad,
                          hash_algorithm) {
      loadNamespace("fst")
      super$initialize(path, compress, mangle_key, mangle_key_pad,
                       hash_algorithm)
    },
    
    type = function() {
      "fst"
    },

    get_object = function(hash) {
      read_fst_value(self$name_hash(hash), self$compress)
    },

    set_object = function(hash, value) {
      ## NOTE: this takes advantage of having the serialized value
      ## already and avoids seralising twice.
      assert_raw(value)
      write_fst_value(value, self$name_hash(hash), self$compress,
                           self$path_scratch)
    },

    list_hashes = function() {
      sub("\\.fst$", "", dir(file.path(self$path, "data")))
    },

    check_objects = function(full, hash_length, progress) {
      check_fst_objects(self, full, hash_length, progress)
    },

    name_hash = function(hash) {
      if (length(hash) > 0L) {
        file.path(self$path, "data", paste0(hash, ".fst"))
      } else {
        character(0)
      }
    }
  ))

read_fst_value <- function(path, compress) {
  if (!file.exists(path)) {
    stop(sprintf("fst file '%s' missing", path))
  }
  out <- fst::read_fst(path)[[1]]
  if (compress) out <- decompress_fst(out)
  unserialize(out)
}

write_fst_value <- function(value, filename, compress,
                                 scratch_dir = NULL) {
  withCallingHandlers(
    try_write_fst_value(value, filename, compress, scratch_dir),
    error = function(e) unlink(filename))
}

try_write_fst_value <- function(value, filename, compress,
                                scratch_dir = NULL) {
  tmp <- tempfile(tmpdir = scratch_dir %||% tempdir())
  if (compress) value <- fst::compress_fst(value)

  fst::write_fst(structure(list(value = value), class = "data.frame"), tmp)
  file.rename(tmp, filename)
}

check_fst_objects <- function (dr, full, hash_length, progress) {
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
        suppressWarnings(fst::read_fst(x, to = 1L))
        FALSE
      }, error = note_error)
    }
    n <- length(files)
    if (progress && n > 0 && requireNamespace("progress", 
                                              quietly = TRUE)) {
      tick <- progress::progress_bar$new(format = "[:spin] [:bar] :percent (:errs corrupt)", 
                                         total = n)$tick
    }
    else {
      tick <- function(...) NULL
    }
    err <- logical(length(files))
    for (i in seq_along(files)) {
      err[i] <- check(files[[i]])
      tick(tokens = list(errs = errs))
    }
  }
  else {
    err <- file_size(files) == 0L
  }
  list(corrupt = h[err])
}

