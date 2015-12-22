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
##
## TODO: are the export lines here actually needed?
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
  for (i in seq_along(list)) {
    dest <- do_set(dest, names_dest[[i]], namespace,
                   do_get(src, list[[i]], namespace))
  }
  list(names=names_dest, dest=dest)
}

do_get <- function(x, key, namespace) {
  UseMethod("do_get")
}
##' @export
do_get.default <- function(x, key, namespace) {
  if (!is.function(x$get)) {
    stop("No suitable method found")
  }
  x$get(key, namespace=namespace)
}
##' @export
do_get.environment <- function(x, key, namespace) {
  x[[key]]
}
##' @export
do_get.list <- do_get.environment

do_set <- function(x, key, namespace, value) {
  UseMethod("do_set")
}
##' @export
do_set.default <- function(x, key, namespace, value) {
  if (!is.function(x$set)) {
    stop("No suitable method found")
  }
  x$set(key, value, namespace=namespace)
  x
}
##' @export
do_set.environment <- function(x, key, namespace, value) {
  assign(key, value, x)
  x
}
##' @export
do_set.list <- function(x, key, namespace, value) {
  x[[key]] <- value
  x
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

##' @export
as.list.storr <- function(x, ...) {
  x <- x$export(list())
  x
}
