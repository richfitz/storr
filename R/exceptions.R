KeyError <- function(key, driver) {
  structure(list(key=key,
                 message=sprintf("key '%s' not found", key),
                 call=NULL),
            class=c("KeyError", "error", "condition"))
}

HashError <- function(hash, driver) {
  structure(list(hash=hash,
                 message=sprintf("hash '%s' not found", hash),
                 call=NULL),
            class=c("HashError", "error", "condition"))
}
