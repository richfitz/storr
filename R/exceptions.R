KeyError <- function(key, namespace, driver) {
  structure(list(key=key,
                 namespace=namespace,
                 message=sprintf("key '%s' ('%s') not found", key, namespace),
                 call=NULL),
            class=c("KeyError", "error", "condition"))
}

KeyErrorExternal <- function(key, namespace, e) {
  msg <- sprintf("key '%s' ('%s') not found, with error: %s",
                 key, namespace, e$message)
  structure(list(key=key,
                 namespace=namespace,
                 message=msg,
                 call=NULL,
                 error=e),
            class=c("KeyErrorExternal", "KeyError", "error", "condition"))
}

HashError <- function(hash, driver) {
  structure(list(hash=hash,
                 message=sprintf("hash '%s' not found", hash),
                 call=NULL),
            class=c("HashError", "error", "condition"))
}
