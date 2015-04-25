hash_object <- function(x) {
  digest::digest(x)
}

exists0 <- function(name, envir) {
  exists(name, envir, inherits=FALSE)
}

rm0 <- function(list, envir) {
  del <- exists0(list, envir)
  rm(list=list[del], envir=envir)
  del
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
