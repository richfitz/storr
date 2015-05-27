list_check_range <- function(key, i, len) {
  err <- i < 1 | i > len
  if (any(err)) {
    stop(IndexError(key, i[err]))
  }
}

list_key <- function(type, key, namespace) {
  hash_object(c(type, namespace, key))
}

list_attributes <- function(x) {
  ret <- attributes(x)
  ret$names <- NULL
  ret
}
