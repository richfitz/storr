##' Create an object cache; a "storr".  A storr is a simple key-value
##' store where the actual content is stored in a content-addressible
##' way (so that duplicate objects are only stored once) and with a
##' caching layer so that repeated lookups are fast even if the
##' underlying storage driver is slow.
##'
##' To create a storr you need to provide a "driver" object.  There
##' are three in this package: \code{\link{driver_environment}} for
##' ephemeral in-memory storage, \code{\link{driver_rds}} for
##' serialized storage to disk, and \code{\link{driver_dbi}} for use
##' with DBI-compliant database interfaces.  The \code{redux} package
##' (on CRAN) provides a storr driver that uses Redis.
##'
##' There are convenience functions (e.g.,
##' \code{\link{storr_environment}} and \code{\link{storr_rds}}) that
##' may be more convenient to use than this function.
##'
##' Once a storr has been made it provides a number of methods.
##' Because storr uses \code{R6} (\code{\link{R6Class}}) objects, each
##' method is accessed by using \code{$} on a storr object (see the
##' examples).  The methods are described below in the "Methods"
##' section.
##'
##' The \code{default_namespace} affects all methods of the storr
##' object that refer to namespaces; if a namespace is not given, then
##' the action (get, set, del, list, import, export) will affect the
##' \code{default_namespace}.  By default this is \code{"objects"}.
##'
##' @template storr_methods
##'
##' @title Object cache
##'
##' @param driver A driver object
##'
##' @param default_namespace Default namespace to store objects in.
##'   By default \code{"objects"} is used, but this might be useful to
##'   have two different \code{storr} objects pointing at the same
##'   underlying storage, but storing things in different namespaces.
##'
##' @export
##' @examples
##' st <- storr(driver_environment())
##' ## Set "mykey" to hold the mtcars dataset:
##' st$set("mykey", mtcars)
##' ## and get the object:
##' st$get("mykey")
##' ## List known keys:
##' st$list()
##' ## List hashes
##' st$list_hashes()
##' ## List keys in another namespace:
##' st$list("namespace2")
##' ## We can store things in other namespaces:
##' st$set("x", mtcars, "namespace2")
##' st$set("y", mtcars, "namespace2")
##' st$list("namespace2")
##' ## Duplicate data do not cause duplicate storage: despite having three
##' ## keys we only have one bit of data:
##' st$list_hashes()
##' st$del("mykey")
##'
##' ## Storr objects can be created that have a default namespace that is
##' ## not "objects" by using the \code{default_namespace} argument (this
##' ## one also points at the same memory as the first storr).
##' st2 <- storr(driver_environment(st$driver$envir),
##'              default_namespace = "namespace2")
##' ## All functions now use "namespace2" as the default namespace:
##' st2$list()
##' st2$del("x")
##' st2$del("y")
storr <- function(driver, default_namespace = "objects") {
  R6_storr$new(driver, default_namespace)
}


##' @importFrom R6 R6Class
R6_storr <- R6::R6Class(
  "storr",
  public = list(
    driver = NULL,
    envir = NULL,
    default_namespace = NULL,
    traits = NULL,

    ## Utility things, to fill later:
    hash_raw = NULL,
    serialize_object = NULL,

    initialize = function(driver, default_namespace) {
      self$driver <- driver
      self$envir <- new.env(parent = emptyenv())
      self$default_namespace <- default_namespace
      self$traits <- storr_traits(driver$traits)
      ## These control what we send and recieve from the drivers
      self$hash_raw <-
        make_hash_serialized_object(driver$hash_algorithm,
                                    !self$traits$drop_r_version)
      self$serialize_object <-
        make_serialize_object(self$traits$drop_r_version,
                              self$traits$accept == "string")
    },

    destroy = function() {
      self$driver$destroy()
      self$driver <- NULL
    },

    flush_cache = function() {
      rm(list = ls(self$envir, all.names = TRUE), envir = self$envir)
    },

    set = function(key, value, namespace = self$default_namespace,
                   use_cache = TRUE) {
      hash <- self$set_value(value, use_cache)
      self$driver$set_hash(key, namespace, hash)
      invisible(hash)
    },

    mset = function(key, value, namespace = self$default_namespace,
                    use_cache = TRUE) {
      assert_length(value, check_length(key, namespace))
      hash <- self$mset_value(value, use_cache)
      storr_mset_hash(self, key, namespace, hash)
      invisible(hash)
    },

    set_by_value = function(value, namespace = self$default_namespace,
                            use_cache = TRUE) {
      hash <- self$set_value(value, use_cache)
      self$driver$set_hash(hash, namespace, hash)
      invisible(hash)
    },

    mset_by_value = function(value, namespace = self$default_namespace,
                             use_cache = TRUE) {
      hash <- self$mset_value(value, use_cache)
      storr_mset_hash(self, hash, namespace, hash)
      invisible(hash)
    },

    get = function(key, namespace = self$default_namespace, use_cache = TRUE) {
      assert_scalar_character(key)
      assert_scalar_character(namespace)
      self$get_value(self$get_hash(key, namespace), use_cache)

    },

    mget = function(key, namespace = self$default_namespace, use_cache = TRUE,
                    missing = NULL) {
      self$mget_value(self$mget_hash(key, namespace), use_cache, missing)
    },

    get_hash = function(key, namespace = self$default_namespace) {
      if (self$traits$throw_missing) {
        tryCatch(self$driver$get_hash(key, namespace),
                 error = function(e) stop(KeyError(key, namespace)))
      } else {
        if (self$exists(key, namespace)) {
          self$driver$get_hash(key, namespace)
        } else {
          stop(KeyError(key, namespace))
        }
      }
    },

    mget_hash = function(key, namespace = self$default_namespace) {
      n <- check_length(key, namespace)
      if (is.null(self$driver$mget_hash)) {
        m <- cbind(key, namespace)
        vcapply(seq_len(n), function(i)
          tryCatch(self$get_hash(m[i, 1L], m[i, 2L]),
                   KeyError = function(e) NA_character_),
          USE.NAMES = FALSE)
      } else {
        ## TODO: this skips the throw_missing branch because
        self$driver$mget_hash(key, namespace)
      }
    },

    del = function(key, namespace = self$default_namespace) {
      n <- check_length(key, namespace)
      invisible(self$driver$del_hash(key, namespace))
    },

    ## NOTE: No del_hash exposed here; this is because otherwise
    ## things can get pretty messy when actual data is being deleted.

    duplicate = function(key_src, key_dest, namespace = self$default_namespace,
                         namespace_src = namespace,
                         namespace_dest = namespace) {
      hash_src <- self$mget_hash(key_src, namespace_src)
      storr_mset_hash(self, key_dest, namespace_dest, hash_src)
    },

    fill = function(key, value, namespace = self$default_namespace,
                    use_cache = TRUE) {
      n <- check_length(key, namespace)
      hash <- self$set_value(value, use_cache)
      storr_mset_hash(self, key, namespace, rep(hash, n))
      invisible(hash)
    },

    clear = function(namespace = self$default_namespace) {
      if (is.null(namespace)) {
        invisible(sum(viapply(self$list_namespaces(), self$clear)))
      } else {
        invisible(length(vlapply(self$list(namespace), self$del, namespace)))
      }
    },

    exists = function(key, namespace = self$default_namespace) {
      self$driver$exists_hash(key, namespace)
    },

    exists_object = function(hash) {
      self$driver$exists_object(hash)
    },

    gc = function() {
      storr_gc(self$driver, self$envir)
    },

    get_value = function(hash, use_cache = TRUE) {
      envir <- self$envir
      if (use_cache && exists0(hash, envir)) {
        value <- envir[[hash]]
      } else {
        if (self$traits$throw_missing) {
          value <- tryCatch(self$driver$get_object(hash),
                            error = function(e) stop(HashError(hash)))
        } else {
          if (!self$driver$exists_object(hash)) {
            stop(HashError(hash))
          }
          value <- self$driver$get_object(hash)
        }
        if (use_cache) {
          envir[[hash]] <- value
        }
      }
      value
    },

    mget_value = function(hash, use_cache = TRUE, missing = NULL) {
      envir <- self$envir
      value <- vector("list", length(hash))
      cached <- logical(length(hash))
      is_missing <- is.na(hash)

      if (use_cache) {
        i <- vlapply(hash, exists0, envir)
        value[i] <- lapply(hash[i], function(h) envir[[h]])
        cached[i] <- TRUE
      }

      cached[is_missing] <- TRUE
      value[is_missing] <- list(missing)

      if (any(!cached)) {
        if (is.null(self$driver$mget_object)) {
          value[!cached] <- lapply(hash[!cached], self$get_value, FALSE)
        } else {
          value[!cached] <- self$driver$mget_object(hash[!cached])
        }

        if (use_cache) {
          for (i in which(!cached)) {
            envir[[hash[[i]]]] <- value[[i]]
          }
        }
      }

      if (any(is_missing)) {
        attr(value, "missing") <- which(is_missing)
      }
      value
    },

    set_value = function(value, use_cache = TRUE) {
      value_ser <- self$serialize_object(value)
      hash <- self$hash_raw(value_ser)

      if (!(use_cache && exists0(hash, self$envir))) {
        ## NOTE: This exists/set roundtrip here always seems useful to
        ## avoid sending (potentially large) data over a connection, but
        ## it's possible that some drivers could do this more
        ## efficiently themselves during negotiation.
        if (!self$driver$exists_object(hash)) {
          value_send <- if (self$traits$accept == "object") value else value_ser
          self$driver$set_object(hash, value_send)
        }
        if (use_cache) {
          assign(hash, value, self$envir)
        }
      }
      invisible(hash)
    },

    mset_value = function(values, use_cache = TRUE) {
      values_ser <- lapply(values, self$serialize_object)
      hash <- vcapply(values_ser, self$hash_raw)
      cached <- logical(length(hash))

      if (use_cache) {
        cached <- vlapply(hash, exists0, self$envir)
        upload <- logical(length(hash))
        upload[!cached] <- !self$driver$exists_object(hash[!cached])
      } else {
        cached <- logical(length(hash))
        upload <- !self$driver$exists_object(hash)
      }

      if (any(upload)) {
        send <- if (self$traits$accept == "object") values else values_ser
        if (is.null(self$driver$mset_object)) {
          for (i in which(upload)) {
            self$driver$set_object(hash[[i]], send[[i]])
          }
        } else {
          self$driver$mset_object(hash[upload], send[upload])
        }
      }

      if (use_cache) {
        for (i in which(!cached)) {
          assign(hash[[i]], values[[i]], self$envir)
        }
      }
      invisible(hash)
    },

    ## We guarantee key sort here; underlying driver does not have to.
    list = function(namespace = self$default_namespace) {
      sort(self$driver$list_keys(namespace))
    },

    list_hashes = function() {
      sort(self$driver$list_hashes())
    },

    list_namespaces = function() {
      sort(self$driver$list_namespaces())
    },

    import = function(src, list = NULL, namespace = self$default_namespace,
                      skip_missing = FALSE) {
      if (is.null(namespace)) {
        if (inherits(src, "storr")) {
          namespace <- src$list_namespaces()
        } else {
          stop("If src is not a storr, namespace can't be NULL")
        }
      }
      invisible(storr_copy(self, src, list, namespace, skip_missing)$info)
    },

    export = function(dest, list = NULL, namespace = self$default_namespace,
                      skip_missing = FALSE) {
      if (is.null(namespace)) {
        namespace <- self$list_namespaces()
      }
      invisible(storr_copy(dest, self, list, namespace, skip_missing)$dest)
    },

    archive_export = function(path, names = NULL, namespace = NULL) {
      self$export(storr_rds(path, mangle_key = TRUE), names, namespace)
    },

    archive_import = function(path, names = NULL, namespace = NULL) {
      self$import(storr_rds(path, mangle_key = TRUE), names, namespace)
    },

    index_export = function(namespace = NULL) {
      storr_index_export(self, namespace)
    },

    index_import = function(index) {
      storr_index_import(self, index)
    },

    check = function(full = TRUE, quiet = FALSE, progress = !quiet) {
      storr_check(self, full, quiet, progress)
    },

    repair = function(storr_check_results = NULL, quiet = FALSE, ...,
                      force = FALSE) {
      storr_repair(self, storr_check_results, quiet, ..., force = force)
    },

    ## Utility function that will come in useful in a few places:
    hash_object = function(object) {
      self$hash_raw(self$serialize_object(object))
    }
  ))


storr_mset_hash <- function(obj, key, namespace, hash) {
  if (is.null(obj$driver$mset_hash)) {
    n <- length(hash)
    key <- rep_len(key, n)
    namespace <- rep_len(namespace, n)
    for (i in seq_len(n)) {
      obj$driver$set_hash(key[[i]], namespace[[i]], hash[[i]])
    }
  } else {
    obj$driver$mset_hash(key, namespace, hash)
  }
}


storr_check <- function(obj, full, quiet, progress) {
  obj$flush_cache()

  driver <- obj$driver
  if (is.null(driver$check_objects) || is.null(driver$check_keys)) {
    stop(sprintf("This storr (with driver type '%s') does not support checking",
                 obj$driver$type()))
  }

  hash_length <- nchar(obj$hash_object(NULL))
  healthy <- TRUE

  if (!quiet) {
    message("Checking objects")
  }
  objects <- obj$driver$check_objects(full, hash_length, progress)
  n <- length(objects$corrupt)
  healthy <- n == 0L
  if (!quiet) {
    if (n > 0L) {
      message(sprintf("...found %d corrupt objects", n))
    } else {
      message("...OK")
    }
  }

  if (!quiet) {
    message("Checking keys")
  }

  keys <- obj$driver$check_keys(full, hash_length, progress, objects$corrupt)
  n <- viapply(keys, nrow)
  healthy <- healthy && all(n == 0L)

  if (!quiet) {
    if (any(n > 0L)) {
      for (i in seq_along(n)[n > 0]) {
        message(sprintf("...found %d %s keys in %d namespaces",
                        n[[i]], names(keys)[[i]],
                        length(unique(keys[[i]][, "namespace"]))))
      }
    } else {
      message("...OK")
    }
  }

  ret <- list(healthy = healthy, objects = objects, keys = keys)
  class(ret) <- "storr_check"
  ret
}


storr_repair <- function(obj, storr_check_results, quiet, ..., force) {
  if (is.null(storr_check_results)) {
    storr_check_results <- obj$check(..., quiet = quiet)
    if (storr_check_results$healthy) {
      return(FALSE)
    }

    msg <- "Delete corrupted data? (no going back!)"
    continue <- force || (interactive() && prompt_ask_yes_no(msg))
    if (!continue) {
      stop("Please rerun with force = TRUE, or provide check_results")
    }
  }
  assert_is(storr_check_results, "storr_check")

  ## Move the logic from above into here.
  if (storr_check_results$healthy) {
    return(FALSE)
  }

  h <- storr_check_results$objects$corrupt
  k <- c(storr_check_results$keys$corrupt[, "key"],
         storr_check_results$keys$dangling[, "key"])
  ns <- c(storr_check_results$keys$corrupt[, "namespace"],
          storr_check_results$keys$dangling[, "namespace"])

  if (length(h) > 0L) {
    if (!quiet) {
      message(sprintf("Deleting %d corrupt objects", length(h)))
    }
    obj$driver$del_object(h)
  }
  if (length(k) > 0L) {
    if (!quiet) {
      message(sprintf("Deleting %d corrupt/dangling keys", length(k)))
    }
    obj$driver$del_hash(k, ns)
  }

  length(h) > 0 || length(k) > 0
}


##' @export
as.list.storr <- function(x, ...) {
  x <- x$export(list(), ...)
  x
}


## This one is complicated enough to come out.
storr_gc <- function(driver, envir) {
  unused <- setdiff(driver$list_hashes(), storr_used_hashes(driver))
  ## clean up unused hashes
  driver$del_object(unused)
  rm0(unused, envir)
  ## and let us know what was removed:
  invisible(unused)
}


storr_used_hashes <- function(driver) {
  list_hashes <- function(ns) {
    if (is.null(driver$mget_hash)) {
      unique(vcapply(driver$list_keys(ns), driver$get_hash, ns,
                     USE.NAMES = FALSE))
    } else {
      unique(driver$mget_hash(driver$list_keys(ns), ns))
    }
  }
  unique(unlist(lapply(driver$list_namespaces(), list_hashes),
                use.names = FALSE))
}


check_length <- function(key, namespace) {
  n_key <- length(key)
  n_namespace <- length(namespace)
  if (n_key == n_namespace || n_namespace == 1) {
    n_key
  } else if (n_key == 1) {
    n_namespace
  } else {
    stop("Incompatible lengths for key and namespace")
  }
}


##' Utility function for driver authors
##'
##' This exists to join, predictably, keys and namespaces for
##' operations like \code{mget}.  Given a vector or scalar for
##' \code{key} and \code{namespace} we work out what the required
##' length is and recycle \code{key} and \code{namespace} to the
##' appropriate length.
##' @title Recycle key and namespace
##' @param key A vector of keys
##' @param namespace A vector of namespace
##' @return A list with elements \code{n}, \code{key} and \code{namespace}
##' @export
join_key_namespace <- function(key, namespace) {
  n <- check_length(key, namespace)
  list(n = n,
       key = rep_len(key, n),
       namespace = rep_len(namespace, n))
}


storr_index_export <- function(st, namespace = NULL) {
  namespace <- namespace %||% st$list_namespaces()
  keys <- lapply(namespace, st$list)
  len <- lengths(keys)
  if (any(len > 0L)) {
    key <- unlist(keys, use.names = FALSE)
  } else {
    key <- character(0)
  }

  ret <- data.frame(namespace = rep(namespace, len),
                    key = key,
                    stringsAsFactors = FALSE)
  ret$hash <- st$mget_hash(ret$key, ret$namespace)
  ret
}


storr_index_import <- function(st, index) {
  cols <- c("namespace", "key", "hash")
  msg <- setdiff(cols, names(index))
  if (length(msg) > 0L) {
    stop("Missing required columns for index: ",
         paste(squote(msg), collapse = ", "),
         call. = FALSE)
  }
  ok <- vlapply(index[cols], is.character)
  if (!all(ok)) {
    stop("Column not character: ", paste(squote(cols[!ok]), collapse = ", "),
         call. = FALSE)
  }
  msg <- setdiff(index$hash, st$list_hashes())
  if (length(msg) > 0L) {
    stop(sprintf("Missing %d / %d hashes - can't import",
                 length(msg), nrow(index)),
         call. = FALSE)
  }
  storr_mset_hash(st, index$key, index$namespace, index$hash)
}
