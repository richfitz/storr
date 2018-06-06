##' Object cache driver that saves objects using R's native
##' serialized file format (see \code{\link{saveRDS}}) on and S3 bucket.
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
storr_rds_s3 <- function(bucket, path, compress = NULL, mangle_key = NULL,
                         mangle_key_pad = NULL, hash_algorithm = NULL,
                         default_namespace = "objects") {
  storr(driver_rds_s3(bucket, path, compress, mangle_key, mangle_key_pad, hash_algorithm),
        default_namespace)
}

##' @export
##' @rdname storr_rds
driver_rds_s3 <- function(bucket, path, compress = NULL, mangle_key = NULL,
                          mangle_key_pad = NULL, hash_algorithm = NULL) {
  R6_driver_rds_s3$new(bucket, path, compress, mangle_key, mangle_key_pad, hash_algorithm)
}

R6_driver_rds_s3 <- R6::R6Class(
  "driver_rds)s3",
  public = list(
    ## TODO: things like hash_algorithm: do they belong in traits?
    ## This needs sorting before anyone writes their own driver!
    bucket = NULL,
    path = NULL,
    compress = NULL,
    mangle_key = NULL,
    mangle_key_pad = NULL,
    hash_algorithm = NULL,
    traits = list(accept = "raw"),

    initialize = function(bucket, path, compress, mangle_key, mangle_key_pad,
                          hash_algorithm) {

      is_new <- !s3_object_exists(bucket = bucket, path = file.path(path, "config"))
      aws.s3::put_folder(folder = path, bucket = bucket)
      aws.s3::put_folder(folder = file.path(path, "data"), bucket = bucket)
      aws.s3::put_folder(folder = file.path(path, "keys"), bucket = bucket)
      aws.s3::put_folder(folder = file.path(path, "config"), bucket = bucket)
      self$bucket <- bucket
      self$path <- path

      ## This is a bit of complicated dancing around to mantain
      ## backward compatibility while allowing better defaults in
      ## future versions.  I'm writing out a version number here that
      ## future versions of driver_rds can use to patch, warn or
      ## change behaviour with older versions of the storr.
      if (!is_new && !s3_object_exists(path = driver_rds_s3_config_file(path, "version"), bucket = bucket)) {
        s3_write_if_missing("1.0.1", bucket = bucket, path = driver_rds_s3_config_file(path, "version"))
        s3_write_if_missing("TRUE", bucket = bucket, path =  driver_rds_s3_config_file(path, "mangle_key_pad"))
        s3_write_if_missing("TRUE", bucket = bucket, path =  driver_rds_s3_config_file(path, "compress"))
        s3_write_if_missing("md5", bucket = bucket, path =  driver_rds_s3_config_file(path, "hash_algorithm"))
      }
      ## Then write out the version number:
      s3_write_if_missing(as.character(packageVersion("storr")),
                          bucket = bucket,
                          path = driver_rds_s3_config_file(path, "version"))

      if (!is.null(mangle_key)) {
        assert_scalar_logical(mangle_key)
      }
      self$mangle_key <- driver_rds_s3_config(bucket, path, "mangle_key", mangle_key,
                                              FALSE, TRUE)

      if (!is.null(mangle_key_pad)) {
        assert_scalar_logical(mangle_key_pad)
      }
      self$mangle_key_pad <-
        driver_rds_s3_config(bucket, path, "mangle_key_pad", mangle_key_pad,
                             FALSE, TRUE)

      if (!is.null(compress)) {
        assert_scalar_logical(compress)
      }
      self$compress <- driver_rds_s3_config(bucket, path, "compress", compress,
                                            TRUE, FALSE)

      if (!is.null(hash_algorithm)) {
        assert_scalar_character(hash_algorithm)
      }
      self$hash_algorithm <- driver_rds_s3_config(bucket, path, "hash_algorithm",
                                                  hash_algorithm, "md5", TRUE)
    },

    type = function() {
      "rds_s3"
    },
    destroy = function() {
      s3_delete_recursive(bucket = self$bucket, path = self$path)
    },

    get_hash = function(key, namespace) {
      s3_readLines(path = self$name_key(key, namespace), bucket = self$bucket)
    },
    set_hash = function(key, namespace, hash) {
      dir_create(self$name_key("", namespace))
      s3_writeLines(text = hash, path = self$name_key(key, namespace), bucket = self$bucket) #*** should be making use of (or making an equivalent version of) the write_lines function within the storr package here (I think it deletes file if the write fails)
    },
    get_object = function(hash) {
      aws.s3::s3readRDS(object = self$name_hash(hash), bucket = self$bucket)
    },
    set_object = function(hash, value) {
      ## NOTE: this takes advantage of having the serialized value
      ## already and avoids seralising twice.
      assert_raw(value)
      aws.s3::s3write_using(x = value, FUN = function(v, p) write_serialized_rds(v, p, self$compress), object = self$name_hash(hash), bucket = self$bucket)
    },

    exists_hash = function(key, namespace) {
      s3_object_exists(self$name_key(key, namespace), bucket = self$bucket)
    },
    exists_object = function(hash) {
      s3_object_exists(self$name_hash(hash), bucket = self$bucket)
    },

    del_hash = function(key, namespace) {
      #s3_delete_file(bucket = self$bucket, path = self$name_key(key, namespace))
      ## above deletes just one file (s3 key).
      ## However it will throw an error if the file we are trying to delete looks like a directory.
      ## S3 has no actual notion of directory, we just fake it using "/". As a result, it's possible to get into a muddle.
      ## to play it save, line below can be uncommented to force it to delete just the path given, but throw a warning, if it does look like a directory
      ## can also change to if_dir = "del_recursive" to delete the whole directory with a warning. May never actually show up as an issue, this is just a note.
      s3_delete_file(bucket = self$bucket, path = self$name_key(key, namespace), if_dir = "del_only_key") # this will throw a warning if your file to be deleted looks like a directory on S3. It will then delete the directory. If this occurs alot for some reason, may want to change behaviour of this delete.

    },
    del_object = function(hash) {
      # see above note which also applies here
      s3_delete_file(bucket = self$bucket, path = self$name_hash(key, namespace), if_dir = "del_only_key")
    },

    list_hashes = function() {
      sub("\\.rds$", "", s3_list_dir(bucket = self$bucket, path = file.path(self$path, "data")))
    },
    list_namespaces = function() {
      s3_list_dir(bucket = self$bucket, path = file.path(self$path, "keys"))
    },
    list_keys = function(namespace) {
      ret <- s3_list_dir(bucket = self$bucket, path = file.path(self$path, "keys", namespace))
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
driver_rds_s3_config <- function(bucket, path, name, value, default, must_agree) {
  path_opt <- driver_rds_s3_config_file(path, name)

  load_value <- function() {
    if (s3_object_exists(bucket, path_opt)) {
      value <- s3_readLines(path_opt, bucket)
      storage.mode(value) <- storage.mode(default)
    } else {
      value <- default
    }
    value
  }

  if (is.null(value)) {
    value <- load_value()
  } else if (must_agree && s3_object_exists(bucket = bucket, path = path_opt)) {
    value_prev <- load_value()
    if (value != value_prev) {
      stop(ConfigError(name, value_prev, value))
    }
  }
  if (!s3_object_exists(bucket = bucket, path = path_opt)) {
    s3_writeLines(text = as.character(value), path = path_opt, bucket = bucket)
  }

  value
}

driver_rds_s3_config_file <- function(path, key) {
  file.path(path, "config", key)
}

s3_write_if_missing <- function(value, bucket, path) {
  if (s3_object_exists(bucket, path)) {
    s3_writeLines(text, path, bucket)
  }
}

## S3 Helper functions

s3_file_remove <- function(path, bucket) {
  exists <- s3_object_exists(bucket, path)
  if (any(exists)) {
    objec(path[exists])
  }
  invisible(exists)
}

s3_writeLines <- function(text, path, bucket){
  aws.s3::s3write_using(x = text, FUN = writeLines, object = path, bucket = bucket)
}

s3_readLines <- function(path, bucket){
  aws.s3::s3read_using(FUN = readLines, object = path, bucket = bucket)
}

s3_object_exists <- function(bucket, path){
  suppressMessages(aws.s3::head_object(object = path, bucket = bucket)[1])
}

s3_list_dir <- function(bucket, path){
  files_table <- aws.s3::get_bucket_df(bucket = bucket, prefix = path, max = Inf)
  keys <- files_table[files_table$Size > 0,]$Key
  files <- gsub(pattern = path, replacement = "", x = keys)
  split_names <- strsplit(files, "/")
  unique(unlist(lapply(split_names, function(x) x[1]))) # first element of each split name is the file or directory within path, take unique of these, so that directories only appear once
}

s3_delete_recursive <- function(bucket, path, force=FALSE){
  files <- aws.s3::get_bucket_df(bucket = bucket, prefix = path, max = Inf)[["Key"]]
  invisible(lapply(files, function(x) aws.s3::delete_object(x, bucket)))
}

s3_delete_file <- function(bucket, path, if_dir = c("stop", "del_only_key", "del_recursive")){
  files <- aws.s3::get_bucket_df(bucket = bucket, prefix = path, max = Inf)[["Key"]]
  if(length(files) > 1){
    if_dir == match.arg(if_dir) # only need this if we get inside this loop
    if(if_dir == "stop"){
      stop("You are trying to delete 1 file, but it looks like it is setup like a directory")
    }
    if(if_dir == "del_only_key"){
      warning("You are trying to delete 1 file, but it looks like it is setup like a directory. Deleted specific path you requested")
      invisible(aws.s3::delete_object(object = path, bucket = bucket))
    }
    if(if_dir == "del_recursive"){
      warning("You are trying to delete 1 file, but it looks like it is setup like a directory. Deleting recursively everyting below the path you specified")
      s3_delete_recursive(bucket, path)
    }
  }
  else{
    invisible(aws.s3::delete_object(object = path, bucket = bucket))
  }
}