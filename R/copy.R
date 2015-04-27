## the aim here is not to be as fast as possible or to avoid as many
## copies as possible so much as be relatively general, consistent and
## easy to extend.
##
## S3 looks nice because we can abstract over assign / $set but it
## doesn't actually work that well.
##
## We can overload [[ for storr but that breaks other things,
## so instead the Rube Goldberg machine is for environments giving a
## fake $get method that redirects to [[.
##
## TODO: namespace here assumes we're copy from and to the same
## namespace.  That's not always going to be reasonable but will have
## to do for now.
storr_copy <- function(dest, src, list, namespace) {
  if (is.null(list)) {
    if (inherits(src, "storr")) {
      list <- src$list(namespace)
    } else if (is.environment(src)) {
      list <- ls(src, all.names=TRUE)
    } else if (is.list(src)) {
      list <- names(src)
    } else {
      stop("Invalid type for src")
    }
  }

  names_dest <- export_names(list)

  do_get <- make_get(src, namespace)
  do_set <- make_set(dest, namespace)
  for (i in seq_along(list)) {
    do_set(names_dest[[i]], do_get(list[[i]]))
  }

  invisible(names_dest)
}

make_get <- function(x, ...) {
  UseMethod("make_get")
}
##' @export
make_get.default <- function(x, namespace, ...) {
  if (!is.function(x$get)) {
    stop("No suitable method found")
  }
  force(namespace)
  function(i) x$get(i, namespace=namespace)
}
##' @export
make_get.environment <- function(x, ...) {
  force(x)
  function(i) x[[i]]
}
##' @export
make_get.list <- make_get.environment

make_set <- function(x, ...) {
  UseMethod("make_set")
}
##' @export
make_set.default <- function(x, namespace, ...) {
  if (!is.function(x$set)) {
    stop("No suitable method found")
  }
  force(namespace)
  function(i, value) x$set(i, value, namespace=namespace)
}
##' @export
make_set.environment <- function(x, ...) {
  force(x)
  function(i, value) assign(i, value, x)
}

## Organise a set of source / destination names.
export_names <- function(list) {
  if (is.null(names(list))) {
    names_out <- list
  } else {
    names_out <- names(list)
    names_out[names_out == ""] <- list[names_out == ""]
  }
  names_out
}
