## All the bits related to object hashing and serialisation
make_hash_serialized_object <- function(hash_algorithm, skip_version) {
  hash <- digest::digest
  hash_algorithm <- hash_algorithm %||% "md5"
  skip <- if (skip_version) 14L else 0L
  function(x) {
    hash(x, hash_algorithm, skip = skip, serialize = FALSE)
  }
}


make_serialize_object <- function(drop_r_version, string, xdr = TRUE,
                                  r_version = get_r_version()) {
  if (string) {
    if (drop_r_version) {
      stop("Can't combine drop_r_version and string serialization")
    }
    ## I really want the ascii = NA form of string serialization
    ## because it is safer with respect to precision loss in doubles.
    ## It's the only thing I know of that depends on R between 3.1 and
    ## 3.2 and affects only the dbi driver at present.
    if (r_version < numeric_version("3.2.0")) {
      stop("Please upgrade R to at least 3.2.0")
    }
    function(object) rawToChar(serialize_to_raw(object, NA, xdr))
  } else if (drop_r_version) {
    function(object) serialize_object_drop_r_version(object, xdr)
  } else {
    function(object) serialize_to_raw(object, FALSE, xdr)
  }
}


unserialize_safe <- function(x) {
  if (is.character(x)) {
    unserialize(charToRaw(x))
  } else if (is.raw(x)) {
    unserialize(x)
  } else {
    stop("Invalid input")
  }
}


## This is needed to support the case where the hash must apply to the
## *entire* structure, just just the relevant bytes.
STORR_R_VERSION_BE <- as.raw(c(0L, 3L, 2L, 0L))
STORR_R_VERSION_LE <- as.raw(c(0L, 2L, 3L, 0L))
serialize_object_drop_r_version <- function(object, xdr = TRUE) {
  dat <- serialize_to_raw(object, FALSE, xdr)
  dat[7:10] <- if (xdr) STORR_R_VERSION_BE else STORR_R_VERSION_LE
  dat
}


serialize_to_raw <- function(x, ascii, xdr) {
  serialize(x, NULL, ascii = ascii, xdr = xdr, version = 2L)
}


## For current R (3.3.2 or thereabouts) writeBin does not work with
## long vectors.  We can work around this for now, but in future
## versions this will just use native R support.
##
## The workaround is to *unserialize* and then use saveRDS to
## serialize directly to a connection.  This is far from ideal, but is
## faster than the previous approach of iterating through the raw
## vector and writing it bit-by-bit to a file (~30s for that approach,
## vs ~10s for this one).
##
## We need to make sure that we only keep the file if the write has
## been successful, otherwise the container will claim existence for
## an object which cannot be retrieved later on, causing havoc
## upstream.
write_serialized_rds <- function(value, filename, compress,
                                 scratch_dir = NULL, long = 2^31 - 2) {
  withCallingHandlers(
    try_write_serialized_rds(value, filename, compress, scratch_dir, long),
    error = function(e) unlink(filename))
}


## The split here helps keep the order really consistent; we will
## close the connection on exit from try_write_serialized_rds and
## delete the file *after* that.
try_write_serialized_rds <- function(value, filename, compress,
                                     scratch_dir = NULL, long = 2^31 - 2) {
  tmp <- tempfile(tmpdir = scratch_dir %||% tempdir())
  if (identical(compress, "fst")) {
    write_tmp_fst(value, tmp)
  } else {
    write_tmp_default(value, tmp, compress, scratch_dir, long)
  }
  file.rename(tmp, filename)
}

write_tmp_fst <- function(value, filename) {
  saveRDS(fst::compress_fst(value), filename, compress = FALSE)
}

write_tmp_default <- function(value, filename, compress, scratch_dir, long) {
  con <- (if (identical(compress, "gzfile")) gzfile else file)(filename, "wb")
  needs_close <- TRUE
  on.exit(if (needs_close) close(con), add = TRUE)
  len <- length(value)
  if (len < long) {
    writeBin(value, con)
  } else {
    message("Repacking large object")
    saveRDS(unserialize(value), con)
  }
  close(con)
  needs_close <- FALSE
}

## Same pattern for write_lines.  The difference is that this will
## delete the key on a failed write (otherwise there's a copy
## involved)
write_lines <- function(text, filename, ..., scratch_dir = NULL) {
  withCallingHandlers(
    try_write_lines(text, filename, ..., scratch_dir = scratch_dir),
    error = function(e) unlink(filename))
}


## This implements write-then-move for writeLines, which gives us
## atomic writes and rewrites.  If 'scratch' is on the same filesystem
## as dirname(filename), then the os's rename is atomic
try_write_lines <- function(text, filename, ..., scratch_dir) {
  tmp <- tempfile(tmpdir = scratch_dir %||% tempdir())
  writeLines(text, tmp, ...)
  ## Not 100% necessary and strictly makes this nonatomic
  unlink(filename)
  file.rename(tmp, filename)
}
