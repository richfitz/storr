list_check_range <- function(key, i, len) {
  err <- i < 1 | i > len
  if (any(err)) {
    stop(IndexError(key, i[err]))
  }
}

list_attr_key <- function(key, namespace) {
  hash_object(c(namespace, key))
}
