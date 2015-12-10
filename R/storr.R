##' Create an object cache; a "storr".  A storr is a simple key-value
##' store where the actual content is stored in a content-addressible
##' way (so that duplicate objects are only stored once) and with a
##' caching layer so that repeated lookups are fast even if the
##' underlying storage driver is slow.
##'
##' To create a storr you need to provide a "driver" object.  There
##' are three in the package: \code{\link{driver_environment}} for
##' ephemeral in-memory storage, \code{\link{driver_rds}} for
##' serialised storage to disk and \code{\link{driver_redis_api}}
##' which stores data in Redis but requires packages that are not on
##' CRAN to function (ropensci/RedisAPI and one of ropensci/rrlite or
##' richfitz/redux).  New drivers are relatively easy to add -- see
##' the "drivers" vignette (\code{vignette("drivers",
##' package="storr")}).
##'
##' Once a storr has been made it provides a number of methods.
##' Because storr uses \code{R6} (\code{\link{R6Class}}) objects, each
##' method is accessed by using \code{$} on a storr object (see the
##' examples).  The methods are described below in the "Methods"
##' section.
##'
##' The \code{default_namespace} and \code{mangle_key} argument change
##' the methods of the storr object.  Setting the
##' \code{default_namespace} argument will mean that in the generated
##' storr, the default namespace will be something other than
##' "objects".  This can save typing.  The method that is used here
##' (currently argument rewriting) may change in future versions of
##' storr.
##'
##' @template storr_methods
##'
##' @title Object cache
##'
##' @param driver A driver object
##'
##' @param default_namespace Default namespace to store objects in.
##'   By default \code{"objects"} is used, but this might be useful to
##'   have two diffent \code{storr} objects pointing at the same
##'   store, but storing things in different namespaces.  Do not use
##'   namespaces beginning with \code{"storr_"} as these are used by
##'   storr itself and your data may get overwritten (unlikely
##'   though).
##'
##' @param mangle_key Mangle keys?  If TRUE, then the key is run
##'   through a hash function first; this allows storing keys against
##'   names that include characters that might not be supported by the
##'   underlying driver.  At present the hash function mangles the
##'   \emph{string} \code{key} but future versions might allow
##'   mangling the \code{value} of \code{key}.
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
##'              default_namespace="namespace2")
##' ## All functions now use "namespace2" as the default namespace:
##' st2$list()
##' st2$del("x")
##' st2$del("y")
storr <- function(driver, default_namespace="objects", mangle_key=FALSE) {
  .R6_storr(driver, default_namespace, mangle_key)$new()
}

.R6_storr <- function(driver, default_namespace, mangle_key) {
  R6::R6Class(
    "storr",
    public=c(list(driver=driver,
                  envir=new.env(parent=emptyenv())),
             storr_methods(default_namespace, mangle_key)))
}

storr_methods <- function(default_namespace, mangle_key) {
  self <- NULL # avoid warning; we'll resolve this symbol later.
  ret <- list(
    destroy=function() {
      self$driver$destroy()
      self$driver <- NULL
    },
    flush_cache=function() {
      rm(list=ls(self$envir, all.names=TRUE), envir=self$envir)
    },
    ## TODO: delete?
    type=function(key, namespace="objects") {
      if (!self$exists(key, namespace)) {
        "none"
      } else {
        "data"
      }
    },
    set=function(key, value, namespace="objects", use_cache=TRUE) {
      hash <- self$set_value(value, use_cache)
      self$driver$set_hash(key, namespace, hash)
      invisible(hash)
    },
    set_by_value=function(value, namespace="objects", use_cache=TRUE) {
      hash <- self$set_value(value, use_cache)
      self$driver$set_hash(hash, namespace, hash)
      invisible(hash)
    },
    get=function(key, namespace="objects", use_cache=TRUE) {
      self$get_value(self$get_hash(key, namespace), use_cache)
    },
    get_hash=function(key, namespace="objects") {
      if (self$exists(key, namespace)) {
        self$driver$get_hash(key, namespace)
      } else {
        stop(KeyError(key, namespace))
      }
    },
    del=function(key, namespace="objects") {
      invisible(self$driver$del_key(key, namespace))
    },
    exists=function(key, namespace="objects") {
      self$driver$exists_key(key, namespace)
    },
    exists_hash=function(hash) {
      self$driver$exists_hash(hash)
    },
    gc=function() {
      storr_gc(self$driver, self$envir)
    },
    get_value=function(hash, use_cache=TRUE) {
      envir <- self$envir
      if (use_cache && exists0(hash, envir)) {
        value <- envir[[hash]]
      } else {
        ## TODO: Allow drivers to declare that they will throw on
        ## invalid access to save two lookups here.
        if (!self$driver$exists_hash(hash)) {
          stop(HashError(hash))
        }
        value <- self$driver$get_object(hash)
        if (use_cache) {
          envir[[hash]] <- value
        }
      }
      value
    },
    set_value=function(value, use_cache=TRUE) {
      hash <- hash_object(value)
      if (!self$driver$exists_hash(hash)) {
        self$driver$set_object(hash, value)
      }
      if (use_cache && !exists0(hash, self$envir)) {
        assign(hash, value, self$envir)
      }
      invisible(hash)
    },
    ## We guarantee key sort here; underlying driver does not have to.
    list=function(namespace="objects") {
      sort(self$driver$list_keys(namespace))
    },
    list_hashes=function() {
      sort(self$driver$list_hashes())
    },
    list_namespaces=function() {
      sort(self$driver$list_namespaces())
    },

    ## To/from R environments (distinct from the environment driver)
    import=function(src, list=NULL, namespace="objects") {
      storr_copy(self, src, list, namespace)
    },
    ## The logic here is taken from remake's object_store, which is
    ## useful as this is destined to replace that object.
    export=function(dest, list=NULL, namespace="objects") {
      storr_copy(dest, self, list, namespace)
      invisible(dest)
    },

    ## TODO: Deal with all the namespaces at once perhaps
    ##
    ## TODO: decide on default for mangle_key; I *think* mangle makes
    ## most sense because of the possibility of non-filename names, but
    ## not sure in general...
    archive_export=function(path, names=NULL, namespace="objects") {
      self$export(storr_rds(path, mangle_key=TRUE), names, namespace)
    },

    archive_import=function(path, names=NULL, namespace="objects") {
      self$import(storr_rds(path, mangle_key=TRUE), names, namespace)
    })
  if (!missing(default_namespace)) {
    ret <- lapply(ret, modify_defaults, "namespace", default_namespace)
  }
  if (mangle_key) {
    ret <- create_mangled(ret)
  }
  ret
}

## This one is complicated enough to come out.
storr_gc <- function(driver, envir) {
  ns <- driver$list_namespaces()
  hashes <- driver$list_hashes()
  seen <- character(0)
  f <- function(key, namespace) {
    driver$get_hash(key, namespace)
  }
  seen <- unique(unlist(lapply(ns, function(i)
    unique(vcapply(driver$list_keys(i), f, i)))))
  unused <- setdiff(hashes, seen)
  for (h in unused) {
    driver$del_hash(h)
  }
  rm0(unused, envir)
  invisible(unused)
}

create_mangled <- function(dat) {
  brace <- quote(`{`)
  for (i in names(dat)) {
    f <- dat[[i]]
    fun_args <- names(formals(f))
    t <- if (i %in% c("archive_export", "archive_export")) "names" else "key"
    if (t %in% fun_args) {
      if (t == "names") {
        expr <- quote(names <- if (is.null(names)) names else mangle(names))
      } else {
        expr <- quote(key <- mangle(key))
      }
      fun_body <- body(f)
      if (identical(fun_body[[1]], brace)) {
        body(f) <- as.call(c(list(brace, expr), as.list(fun_body[-1])))
      } else {
        body(f) <- as.call(c(list(brace, expr), fun_body))
      }
      dat[[i]] <- f
    } else if (i == "list") {
      fun_body <- body(f)
      fun_body[[2]] <- call("unmangle", fun_body[[2]], TRUE)
      body(f) <- fun_body
      dat[[i]] <- f
    }
  }
  dat
}
