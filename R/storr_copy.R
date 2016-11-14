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
      list <- ls(src, all.names = TRUE)
    } else if (is.list(src)) {
      list <- names(src)
    } else {
      stop("Invalid type for src")
    }
  }

  names_dest <- export_names(list)

  do_get <- make_do_get(src)
  do_set <- make_do_set(dest)

  for (i in seq_along(list)) {
    value <- do_get(list[[i]], namespace)
    dest <- do_set(names_dest[[i]], value, namespace)
  }
  list(names = names_dest, dest = dest)
}

make_do_get <- function(x) {
  if (is.function(x$get)) {
    x$get
  } else if (is.environment(x) || is.list(x)) {
    function(key, namespace) x[[key]]
  } else {
    stop("I don't know how to 'get' from objects of type ",
         paste(class(x), sep = "/"))
  }
}

make_do_set <- function(x) {
  if (is.function(x$set)) {
    function(key, value, namespace) {
      x$set(key, value, namespace)
      x
    }
  } else if (is.environment(x) || is.list(x)) {
    function(key, value, namespace) {
      x[[key]] <- value
      x
    }
  }
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
