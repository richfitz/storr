storr_copy <- function(dest, src, list, namespace, skip_missing) {
  if (length(namespace) > 1L) {
    if (!(is_storr(dest) && is_storr(src))) {
      stop("If exporting multiple namespaces, both dest and src must be storrs")
    }
  }

  namespace_dest <- export_names(namespace)
  namespace_src <- namespace

  info <- vector("list", length(namespace))
  do_mget <- make_do_mget(src)
  do_mset <- make_do_mset(dest)

  if (is.null(list)) {
    if (is_storr(src)) {
      list <- lapply(namespace, src$list)
      i <- rep(seq_along(namespace), lengths(list))
      list <- unlist(list, use.names = FALSE)
      namespace_dest <- namespace_dest[i]
      namespace_src <- namespace_src[i]
    } else if (is.environment(src)) {
      list <- ls(src, all.names = TRUE)
    } else if (is.list(src)) {
      list <- names(src)
    } else {
      ## This is unreachable, in theory, because of the checks in make_do_m*
      stop("Invalid type for src [storr bug] ") # nocov
    }
    names_dest <- list
  } else {
    names_dest <- export_names(list)
  }

  ## TODO: this can be done possibly more efficiently if we work at
  ## the level of the keys and the hashes.  So we get all the hashes
  ## and then set all the values.  This will work best when both
  ## objects are storrs.  OTOH, the storr logic will do the bulk of
  ## the hard work there for us but it will involve rehashing a lot of
  ## stuff needlessly.  It's a bit nasty though because if we're
  ## copying storrs with different traits then the accept raw (or not)
  ## thing comes very important, as does different hash algorithms.
  values <- do_mget(list, namespace_src)
  missing <- attr(values, "missing", exact = TRUE)
  if (!is.null(missing)) {
    if (skip_missing) {
      values <- values[-missing]
      names_dest <- names_dest[-missing]
      namespace_dest <- namespace_dest[-missing]
    } else {
      ## Here we need to find and nicely display all the missing keys,
      ## sorted by namespace.  It's a bit of a faff, really.
      msg <-
        cbind(namespace = namespace_src, name = list)[missing, , drop = FALSE]
      msg <- split(msg[, "name"], msg[, "namespace"])
      msg <- vcapply(names(msg), function(ns)
        sprintf("from namespace %s, %s: %s", squote(ns),
                ngettext(length(msg[[ns]]), "key", "keys"),
                paste(squote(msg[[ns]]), collapse = ", ")),
        USE.NAMES = FALSE)
      msg <- paste0("\t- ", msg, collapse = "\n")
      stop("Missing values; can't copy:\n", msg)
    }
  }

  dest <- do_mset(names_dest, values, namespace_dest)

  info <- cbind(namespace = namespace_dest,
                name = names_dest)
  list(info = info, dest = dest)
}


make_do_mget <- function(src) {
  force(src)
  if (is_storr(src)) {
    src$mget
  } else if (is.environment(src) || is.list(src)) {
    function(key, namespace) {
      lapply(key, function(x) src[[x]])
    }
  } else {
    stop("Invalid type for src; can't 'get' from objects of type ",
         paste(class(src), sep = "/"))
  }
}


make_do_mset <- function(dest) {
  if (is_storr(dest)) {
    function(key, value, namespace) {
      dest$mset(key, value, namespace)
      dest
    }
  } else if (is.environment(dest) || is.list(dest)) {
    function(key, value, namespace) {
      for (i in seq_along(key)) {
        dest[[key[[i]]]] <- value[[i]]
      }
      dest
    }
  } else {
    stop("Invalid type for dest; can't 'set' into objects of type ",
         paste(class(dest), sep = "/"))
  }
}


## Organise a set of source / destination names.
export_names <- function(list) {
  if (is.null(names(list))) {
    names_out <- list
  } else {
    names_out <- names(list)
    i <- !nzchar(names_out)
    names_out[i] <- list[i]
  }
  names_out
}
