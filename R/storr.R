## TODO: Some drivers, such as the Redis ones, and any SQL drivers,
## can have an idea of atomicness that will be very useful to use.  If
## drivers can declare that they have atomic operations then we can do
## the transaction and hopefully pipe line it.
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
  ret <- list(destroy=function() {
    self$driver$destroy()
    self$driver <- NULL
  },
  flush_cache=function() {
    rm(list=ls(self$envir, all.names=TRUE), envir=self$envir)
  },
  type=function(key, namespace="objects") {
    if (!self$exists(key, namespace)) {
      "none"
    } else {
      "data"
    }
  },
  set=function(key, value, namespace="objects", use_cache=TRUE) {
    hash <- hash_object(value)
    self$set_value(hash, value, use_cache)
    self$driver$set_hash(key, namespace, hash)
    invisible(hash)
  },
  set_by_value=function(value, namespace="objects", use_cache=TRUE) {
    hash <- hash_object(value)
    self$set_value(hash, value, use_cache)
    self$driver$set_hash(hash, namespace, hash)
    invisible(hash)
  },
  get=function(key, namespace="objects", use_cache=TRUE) {
    self$get_value(self$get_hash(key, namespace), use_cache)
  },
  ## TODO: use_cache should not be here?
  get_hash=function(key, namespace="objects", use_cache=TRUE) {
    if (self$exists(key, namespace)) {
      self$driver$get_hash(key, namespace)
    } else {
      stop(KeyError(key, namespace))
    }
  },
  del=function(key, namespace="objects") {
    invisible(self$driver$del_key(key, namespace))
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
  set_value=function(hash, value, use_cache=TRUE) {
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

  exists=function(key, namespace="objects") {
    self$driver$exists_key(key, namespace)
  },
  exists_hash=function(hash) {
    self$driver$exists_hash(hash)
  },

  ## To/from R environments (distinct from the environment driver)
  import=function(envir, list=NULL, namespace="objects") {
    storr_copy(self, envir, list, namespace)
  },
  ## The logic here is taken from remake's object_store, which is
  ## useful as this is destined to replace that object.
  export=function(target, list=NULL, namespace="objects") {
    storr_copy(target, self, list, namespace)
  },
  ## A simple convenience function, given that as.environment is not
  ## going to work (it's internal and the mode of an R6 class is
  ## already environment!)
  to_environment=function(parent=.GlobalEnv, list=NULL, namespace="objects") {
    envir <- new.env(parent=parent)
    self$export(envir, list, namespace)
    envir
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
