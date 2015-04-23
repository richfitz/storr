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
