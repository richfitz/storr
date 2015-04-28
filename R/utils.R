hash_object <- function(x) {
  digest::digest(x)
}

hash_string <- function(str) {
  digest::digest(str, serialize=FALSE)
}

exists0 <- function(name, envir) {
  exists(name, envir=envir, inherits=FALSE)
}

exists1 <- function(name, envir) {
  is.environment(envir) && exists0(name, envir)
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

keys_minus_prefix <- function(con, prefix) {
  str_drop_start(as.character(con$KEYS(paste0(prefix, "*"))), prefix)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

assert_scalar <- function (x, name=deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call.=FALSE)
  }
}
assert_length <- function(x, n, name=deparse(substitute(x))) {
  if (length(x) != n) {
    stop(sprintf("%s must have %d elements", name, n), call. = FALSE)
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

## See R.utils::countLines for a promising alternative...
## and http://stackoverflow.com/questions/23456170/get-the-number-of-lines-in-a-text-file-using-r
count_lines <- function(filename) {
  length(readLines(filename))
}

## NOTE: these come from RedisAPI; they're directly here rather than
## as an @import diretive because I want to keep the dependency chain
## polite.  But it means that if either package improves how these are
## handled that needs backporting.
object_to_string <- function(obj) {
  rawToChar(serialize(obj, NULL, TRUE))
}
string_to_object <- function(str) {
  unserialize(charToRaw(str))
}

modify_defaults_R6 <- function(cl, name, argname, default) {
  unlockBinding(name, cl)
  on.exit(lockBinding(name, cl))
  cl[[name]] <- modify_defaults(cl[[name]], "namespace", default)
  invisible()
}

modify_defaults <- function(fun, argname, default) {
  ff <- formals(fun)
  ff[argname] <- default
  replace_formals(fun, ff)
}

## This replaces forms, but preserves attributes except for srcref,
## which will be invalid for any nontrivial change (and will
## confusingly be printed with the wrong structure).
replace_formals <- function(fun, value, envir=environment(fun)) {
  old_attributes <- attributes(fun)
  formals(fun, envir=envir) <- value
  attributes(fun) <- old_attributes[names(old_attributes) != "srcref"]
  fun
}
