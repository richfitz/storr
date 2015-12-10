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

modify_defaults <- function(fun, argname, default) {
  ff <- formals(fun)
  if (argname %in% names(ff)) {
    ff[argname] <- default
    replace_formals(fun, ff)
  } else {
    fun
  }
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

## Might be useful for performance.
declass_R6 <- function(x) {
  if (!is.null(class(x)) && inherits(x, "R6") && !is.null(x$clone)) {
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

assert_scalar <- function(x, name=deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call.=FALSE)
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
