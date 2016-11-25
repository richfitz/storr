hash_object <- function(x, hash_algorithm = "md5", ...) {
  digest::digest(x, algo = hash_algorithm, ...)
}

make_hash_serialised_object <- function(hash_algorithm, skip_version) {
  ## i <- -seq_len(14L) -- if using openssl
  hash <- digest::digest
  hash_algorithm <- hash_algorithm %||% "md5"
  skip <- if (skip_version) 14L else 0L
  function(x) {
    hash(x, hash_algorithm, skip = skip, serialize = FALSE)
  }
}

exists0 <- function(name, envir) {
  vlapply(name, exists, envir = envir, inherits = FALSE, USE.NAMES = FALSE)
}

rm0 <- function(list, envir) {
  del <- exists0(list, envir)
  if (any(del)) {
    rm(list = list[del], envir = envir)
  }
  del
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

assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}
assert_length <- function(x, n, name = deparse(substitute(x))) {
  if (length(x) != n) {
    stop(sprintf("'%s' must have %d elements (recieved %d)",
                 name, n, length(x)), call. = FALSE)
  }
}

assert_function <- function(x, name = deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("'%s' must be a function", name), call. = FALSE)
  }
}

assert_environment <- function(x, name = deparse(substitute(x))) {
  if (!is.environment(x)) {
    stop(sprintf("'%s' must be an environment", name), call. = FALSE)
  }
}
assert_list <- function(x, name = deparse(substitute(x))) {
  if (!is.list(x)) {
    stop(sprintf("'%s' must be a list", name), call. = FALSE)
  }
}

assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be logical", name), call. = FALSE)
  }
}
assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be character", name), call. = FALSE)
  }
}
assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}

assert_raw <- function(x, name = deparse(substitute(x))) {
  if (!is.raw(x)) {
    stop(sprintf("'%s' must be raw", name), call. = FALSE)
  }
}

dir_create <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, FALSE, TRUE)
  }
}

file_remove <- function(path) {
  exists <- file.exists(path)
  if (any(exists)) {
    file.remove(path[exists])
  }
  invisible(exists)
}

## For current R (3.2.3 or thereabouts) writeBin does not work with
## long vectors.  We can work around this for now, but in future
## versions this will just use native R support.
write_bin <- function(value, con, long = 2^31 - 2) {
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

serialize_str <- function(x) {
  rawToChar(serialize(x, NULL, TRUE))
}
unserialize_str <- function(x) {
  unserialize(charToRaw(x))
}

serialize_object <- function(object, xdr = TRUE, drop_r_version = FALSE) {
  if (drop_r_version) {
    serialize_object_drop_r_version(object, xdr)
  } else {
    serialize(object, NULL, xdr = xdr)
  }
}

## This is needed to support the case where the hash must apply to the
## *entire* structure, just just the relevant bytes.
STORR_R_VERSION_BE <- as.raw(c(0L, 3L, 2L, 0L))
STORR_R_VERSION_LE <- as.raw(c(0L, 2L, 3L, 0L))
serialize_object_drop_r_version <- function(object, xdr = TRUE) {
  dat <- serialize(object, NULL, xdr = xdr, version = 2L)
  dat[7:10] <- if (xdr) STORR_R_VERSION_BE else STORR_R_VERSION_LE
  dat
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

squote <- function(x) {
  sprintf("'%s'", x)
}
dquote <- function(x) {
  sprintf('"%s"', x)
}
