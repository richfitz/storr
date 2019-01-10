exists0 <- function(name, envir) {
  vlapply(name, exists, envir = envir, inherits = FALSE, USE.NAMES = FALSE)
}


rm0 <- function(list, envir) {
  del <- exists0(list, envir)
  if (any(del)) {
    rm(list = list[del], envir = envir)
  }
  del
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}


assert_length <- function(x, n, name = deparse(substitute(x))) {
  if (length(x) != n) {
    stop(sprintf("'%s' must have %d elements (recieved %d)",
                 name, n, length(x)), call. = FALSE)
  }
}


assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
}


assert_function <- function(x, name = deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("'%s' must be a function", name), call. = FALSE)
  }
}


assert_environment <- function(x, name = deparse(substitute(x))) {
  if (!is.environment(x)) {
    stop(sprintf("'%s' must be an environment", name), call. = FALSE)
  }
}


assert_list <- function(x, name = deparse(substitute(x))) {
  if (!is.list(x)) {
    stop(sprintf("'%s' must be a list", name), call. = FALSE)
  }
}


assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be logical", name), call. = FALSE)
  }
}


assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}


assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be character", name), call. = FALSE)
  }
}


assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}


assert_raw <- function(x, name = deparse(substitute(x))) {
  if (!is.raw(x)) {
    stop(sprintf("'%s' must be raw", name), call. = FALSE)
  }
}


assert_probably_storr_driver <- function(x, name = deparse(substitute(x))) {
  expected <- c("type", "get_hash", "set_hash", "get_object",
                "set_object", "exists_hash", "exists_object",
                "del_hash", "del_object", "list_hashes",
                "list_keys", "list_namespaces", "type", "destroy")
  ok <- vlapply(expected, function(m) m %in% names(x) && is.function(x[[m]]))
  if (!all(ok)) {
    stop(sprintf("'%s' does not look like a storr driver", name))
  }
  invisible(x)
}


match_value <- function(x, choices, name = deparse(substitute(x))) {
  assert_scalar_character(x, name)
  i <- match(x, choices)
  if (is.na(i)) {
    stop(sprintf("%s must be one of {%s}",
                 name, paste(choices, collapse = ", ")),
         call. = FALSE)
  }
  choices[[i]]
}


dir_create <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, FALSE, TRUE)
  }
}


file_remove <- function(path) {
  exists <- file.exists(path)
  if (any(exists)) {
    file.remove(path[exists])
  }
  invisible(exists)
}


`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


squote <- function(x) {
  sprintf("'%s'", x)
}


dquote <- function(x) {
  sprintf('"%s"', x)
}


is_storr <- function(x) {
  inherits(x, "storr")
}


get_r_version <- function() {
  getRversion()
}


## Not in R until 3.2.x
file_size <- function(...) {
  file.info(..., extra_cols = FALSE)$size
}


prompt_ask_yes_no <- function(reason) {
  utils::menu(c("no", "yes"), FALSE, title = reason) == 2 # nocov
}

#' @useDynLib storr, .registration = TRUE
# Read RDS keys fast
read_text_file <- function(path, nchar) {
  .Call(Cread_text_file, path, nchar)
}


read_rds <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("rds file '%s' missing", path))
  }
  readRDS(path)
}
