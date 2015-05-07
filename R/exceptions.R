KeyError <- function(key, driver) {
  structure(list(key=key,
                 message=sprintf("key '%s' not found", key),
                 call=NULL),
            class=c("KeyError", "error", "condition"))
}

KeyErrorExternal <- function(key, e, driver) {
  msg <- sprintf("key '%s' not found, with error: %s", key, e$message)
  structure(list(key=key,
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

## TODO: allow possible range here?
IndexError <- function(key, index) {
  structure(list(key=key,
                 index=index,
                 message=sprintf("Index %s is out of bounds",
                   paste(index, collapse=", ")),
                 call=NULL),
            class=c("TypeError", "error", "condition"))
}

TypeError <- function(key, expected, recieved, driver) {
  structure(list(key=key,
                 expected=expected,
                 recieved=recieved,
                 message=sprintf("Wrong type: expected %s, recieved %s",
                   expected, recieved),
                 call=NULL),
            class=c("TypeError", "error", "condition"))
}

DownloadError <- function(content) {
  msg <- sprintf("Downloading %s failed with code %d",
                 content$url, httr::status_code(content))
  structure(list(content=content,
                 message=msg,
                 call=NULL),
            class=c("DownloadError", "error", "condition"))
}
