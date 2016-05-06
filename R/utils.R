hash_object <- function(x, ...) {
  digest::digest(x, algo="md5", ...)
}

hash_string <- function(str) {
  digest::digest(str, serialize=FALSE)
}

exists0 <- function(name, envir) {
  exists(name, envir=envir, inherits=FALSE)
}

rm0 <- function(list, envir) {
  if (length(list) > 0L) {
    del <- exists0(list, envir)
    rm(list=list[del], envir=envir)
    del
  }
}

str_drop_start <- function(x, sub) {
  substr(x, nchar(sub) + 1L, nchar(x))
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

assert_scalar <- function(x, name=deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call.=FALSE)
  }
}
assert_length <- function(x, n, name=deparse(substitute(x))) {
  if (length(x) != n) {
    stop(sprintf("%s must have %d elements", name, n), call. = FALSE)
  }
}

assert_function <- function(x, name=deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("%s must be a function", name), call. = FALSE)
  }
}

assert_environment <- function(x, name=deparse(substitute(x))) {
  if (!is.environment(x)) {
    stop(sprintf("%s must be an environment", name), call. = FALSE)
  }
}
assert_list <- function(x, name=deparse(substitute(x))) {
  if (!is.list(x)) {
    stop(sprintf("%s must be a list", name), call. = FALSE)
  }
}

assert_logical <- function(x, name=deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("%s must be logical", name), call.=FALSE)
  }
}
assert_scalar_logical <- function(x, name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}

assert_raw <- function(x, name=deparse(substitute(x))) {
  if (!is.raw(x)) {
    stop(sprintf("%s must be raw", name), call.=FALSE)
  }
}

dir_create <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, FALSE, TRUE)
  }
}

file_remove <- function(path) {
  assert_scalar(path)
  exists <- file.exists(path)
  if (exists) {
    file.remove(path)
  }
  invisible(exists)
}

## Might be useful for performance.
declass_R6 <- function(x) {
  if (!is.null(class(x)) && inherits(x, "R6") && is.function(x$clone)) {
    x <- x$clone()
    class(x) <- NULL
  }
  x
}
reclass_R6 <- function(x, cl) {
  if (is.environment(x) && is.null(class(x))) {
    class(x) <- cl
  }
  x
}

## TODO: This does not support alternative serialisation (yet), or
## dealing with things that have been string serialised (see
## redis_api).  To fix, look at readRDS' source code and work out how
## it determines if the file is the correct format.
RAW_HEADER <- as.raw(c(88L, 10L, 0))
is_serialized <- function(x) {
  is.raw(x) && length(x) > 3L && identical(x[1:3], RAW_HEADER)
}

## For current R (3.2.3 or thereabouts) writeBin does not work with
## long vectors.  We can work around this for now, but in future
## versions this will just use native R support.
write_bin <- function(value, con, long=2^31 - 2) {
  len <- length(value)
  if (len > long) {
    i <- 1L
    while (i < len) {
      j <- i + long - 1L
      writeBin(value[seq(i, min(j, len))], con)
      i <- j + 1L
    }
  } else {
    writeBin(value, con)
  }
}
