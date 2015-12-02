##' Create an object cache
##' @title Object cache
##' @param driver A driver object
##' @param default_namespace Default namespace to store objects in.
##' By default \code{"objects"} is used, but this might be useful to
##' have two diffent \code{storr} objects pointing at the same store,
##' but storing things in different namespaces.  Do not use namespaces
##' beginning with \code{"storr_"} as these are used by storr itself
##' and your data may get overwritten (unlikely though).
##' @param mangle_key Mangle keys?  If TRUE, then the key is run
##' through a hash function first; this allows storing keys against
##' names that include characters that might not be supported by the
##' underlying driver.  At present the hash function mangles the
##' \emph{string} \code{key} but future versions might allow mangling
##' the \code{value} of \code{key}.
##' @export
storr <- function(driver, default_namespace="objects",
                  mangle_key=FALSE) {
  if (mangle_key) {
    make_storr_mangled(driver, default_namespace)
  } else {
    make_storr(driver, default_namespace)
  }
}

## NOTE: See storr_methods for the actual method definitions.  Because
## I want to rewrite some function arguments here there's a bit of a
## faff getting these objects constructed.

##' @importFrom R6 R6Class
make_storr <- function(driver, default_namespace) {
  R6::R6Class(
    "storr",
    public=c(list(driver=driver,
                  envir=new.env(parent=emptyenv())),
             storr_methods(default_namespace)))$new()
}

## The way mangled methods work is we have a new class that implements
## all of the storr methods but mangles the keys and passes them off.
make_storr_mangled <- function(driver, default_namespace) {
  R6::R6Class(
    "storr_mangled",
    public=c(list(storr=make_storr(driver, default_namespace)),
             storr_mangled_methods(default_namespace)))$new()
}

storr_methods <- function(default_namespace) {
  self <- NULL # avoid warning; we'll resolve this symbol later.
  ret <- list(
    type=function(key, namespace="objects") {
      if (!self$driver$exists_key(key, namespace)) {
        "none"
      } else if (self$driver$exists_list(key, namespace)) {
        "list"
      } else {
        "data"
      }
    },

    set=function(key, value, namespace="objects", use_cache=TRUE) {
      ## NOTE: duplicated in driver_external::get_hash
      hash <- hash_object(value)
      self$set_value(hash, value, use_cache)
      self$driver$set_key_hash(key, hash, namespace)
    },
    set_by_value=function(value, namespace="objects", use_cache=TRUE) {
      hash <- hash_object(value)
      self$set_value(hash, value, use_cache)
      self$driver$set_key_hash(hash, hash, namespace)
      invisible(hash)
    },

    get=function(key, namespace="objects", use_cache=TRUE) {
      if (self$driver$exists_list(key, namespace) &&
          !self$driver$exists_key(key, namespace)) {
        value <- self$get_list(key, namespace, use_cache)
        self$set(key, value, namespace, use_cache)
        value
      } else {
        self$get_value(self$get_hash(key, namespace), use_cache)
      }
    },

    get_hash=function(key, namespace="objects") {
      if (self$driver$exists_key(key, namespace)) {
        self$driver$get_hash(key, namespace)
      } else if (self$driver$exists_list(key, namespace)) {
        ## NOTE: This situation happens where a list is modified
        ## (invalidating the overall hash).  So get_hash above fails
        ## above, but we can actually recover here.  Unfortunately
        ## recovery is involved: there's no way of recomputing the
        ## hash without entirely reconstructing the object.
        ##
        ## TODO: Not 100% sure that use_cache here is a good idea.
        use_cache <- TRUE
        value <- self$get_list(key, namespace, use_cache)
        self$set(key, value, namespace, use_cache)
        self$driver$get_hash(key, namespace)
      } else {
        ## This will fall back on the appropriate error:
        self$driver$get_hash(key, namespace)
      }
    },

    del=function(key, namespace="objects") {
      ## TODO: This will report the wrong thing for lists being
      ## deleted much of the time.  Possibly drop due to YAGNI?
      if (self$driver$exists_list(key, namespace)) {
        self$driver$del_key(list_key("attr", key, namespace),
                            namespace="storr_list")
        self$driver$del_key(list_key("names", key, namespace),
                            namespace="storr_list")
        self$driver$del_hash_list(key, namespace)
      }
      invisible(self$driver$del_key(key, namespace))
    },

    ## Totally naive garbage collection.  Should possibly be
    ## replacable by the driver.
    gc=function() {
      ## TODO: need to get all keys from all namespaces; that probably
      ## requires that drivers can report about namespaces.
      ##
      ## Probably best to have drivers do the hard work for us here?
      ns <- "objects"
      keys <- self$list(ns)
      hashes_used1 <- vcapply(keys, self$driver$get_hash,
                              ns, USE.NAMES=FALSE)

      ## TODO: Tidy this mess up!
      lists <- self$driver$list_lists(ns)
      hashes_used2 <- unlist(lapply(lists, self$driver$get_hash_list,
                                    NULL, ns))
      hashes_used3 <- vcapply(lists, function(x)
        self$driver$get_hash(list_key("attr", x, ns), "storr_list"),
                              USE.NAMES=FALSE)
      hashes_used4 <- vcapply(lists, function(x)
        self$driver$get_hash(list_key("names", x, ns), "storr_list"),
                              USE.NAMES=FALSE)
      hashes_used <- unique(c(hashes_used1, hashes_used2,
                              hashes_used3, hashes_used4))

      hashes <- self$list_hashes()
      unused <- setdiff(hashes, hashes_used)
      for (h in unused) {
        self$driver$del_hash(h)
      }
      rm0(unused, self$envir)
      invisible(unused)
    },
    ## TODO: rename clear_cache?  flush_cache?  Make it clear this is
    ## not destructive.
    flush=function() {
      rm(list=ls(self$envir, all.names=TRUE), envir=self$envir)
    },

    get_value=function(hash, use_cache=TRUE) {
      envir <- self$envir
      if (use_cache && exists0(hash, envir)) {
        envir[[hash]]
      } else {
        value <- self$driver$get_value(hash)
        if (use_cache) {
          envir[[hash]] <- value
        }
        value
      }
    },

    set_value=function(hash, value, use_cache=TRUE) {
      ## NOTE: duplicated in driver_external::get_hash
      if (!self$driver$exists_hash(hash)) {
        self$driver$set_hash_value(hash, value)
      }
      if (use_cache && !exists0(hash, self$envir)) {
        assign(hash, value, self$envir)
      }
    },

    ## TODO: These are getting renamed...
    list_hashes=function() {
      self$driver$list_hashes()
    },
    ## NOTE: this is the user-facing set of keys, which might be
    ## different to the driver-reported set of keys because it
    ## includes indexable things.
    list=function(namespace="objects") {
      sort(union(self$driver$list_keys(namespace),
                 self$driver$list_lists(namespace)))
    },

    exists=function(key, namespace="objects") {
      self$driver$exists_key(key, namespace) ||
        self$driver$exists_list(key, namespace)
    },

    ## List support:
    ## TODO: This one *might* be is_list, really?
    exists_list=function(key, namespace="objects") {
      self$driver$exists_list(key, namespace)
    },
    length_list=function(key, namespace="objects") {
      self$driver$length_list(key, namespace)
    },

    set_list=function(key, value, namespace="objects", use_cache=TRUE) {
      self$set_list_elements(key, NULL, value, namespace, use_cache)
      self$set(list_key("attr", key, namespace), list_attributes(value),
               "storr_list", use_cache)
      self$set(list_key("names", key, namespace), names(value),
               "storr_list", use_cache)
      self$set(key, value, namespace, use_cache)
    },
    ## "set list element" requires more care because we need to
    ## invalidate the current list and that requires tweaking get...
    get_list_element=function(key, i, namespace="objects", use_cache=TRUE) {
      assert_scalar(i)
      self$get_value(self$get_list_hash(key, i, namespace), use_cache)
    },
    get_list_elements=function(key, i, namespace="objects", use_cache=TRUE) {
      hash <- self$get_list_hash(key, i, namespace)
      ret <- lapply(hash, self$get_value, use_cache)
      nms <- self$get(list_key("names", key, namespace),
                      "storr_list", use_cache)
      names(ret) <- nms[i]
      ret
    },
    set_list_element=function(key, i, value, namespace="objects", use_cache=TRUE) {
      assert_scalar(i)
      hash <- hash_object(value)
      self$set_value(hash, value, use_cache)
      self$driver$set_key_hash_list(key, i, hash, namespace)
      self$driver$del_key(key, namespace)
    },
    set_list_elements=function(key, i, values, namespace="objects", use_cache=TRUE) {
      if (!is.null(i)) {
        assert_length(i, length(values))
      }
      hash_el <- vcapply(values, hash_object)
      for (j in seq_along(hash_el)) {
        self$set_value(hash_el[[j]], values[[j]], use_cache)
      }
      self$driver$set_key_hash_list(key, i, hash_el, namespace)
      self$driver$del_key(key, namespace)
    },
    get_list=function(key, namespace="objects", use_cache=TRUE) {
      i <- seq_len(self$length_list(key))
      value <- self$get_list_elements(key, i, namespace, use_cache)
      attributes(value) <- self$get(list_key("attr", key, namespace),
                                    "storr_list", use_cache)
      names(value) <- self$get(list_key("names", key, namespace),
                               "storr_list", use_cache)
      value
    },
    get_list_hash=function(key, i=NULL, namespace="objects") {
      if (is.null(i)) {
        i <- seq_len(self$length_list(key))
      }
      self$driver$get_hash_list(key, i, namespace)
    },

    get_list_names=function(key, namespace="objects", use_cache=TRUE) {
      self$get(list_key("names", key, namespace),
               namespace="storr_list", use_cache=use_cache)
    },
    set_list_names=function(key, value, namespace="objects", use_cache=TRUE) {
      self$set(list_key("names", key, namespace), value,
               namespace="storr_list", use_cache=use_cache)
      self$driver$del_key(key, namespace)
    },

    ## TODO: All of these need to support namespaces.

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

    archive_export=function(path, names=NULL, namespace="objects") {
      self$export(storr_rds(path), names, namespace)
    },

    archive_import=function(path, names=NULL, namespace="objects") {
      self$import(storr_rds(path), names, namespace)
    }
  )
  if (!missing(default_namespace)) {
    ret <- lapply(ret, modify_defaults, "namespace", default_namespace)
  }
  ret
}

## Regarding key mangling:
##
##   This is another version that will arrange to mangle key *names*
##   to their hash.  This results in a store where we cannot determine
##   the keys for what is stored in it!
##
##   This is pretty evil code generation.  There may be a better way,
##   but this avoids a lot of nasty repetition.  What it does is looks
##   at the body of the call and adds a hash_string(key) around all
##   keys that are used.
##
##   It might be better to do this at the *driver* level; the only
##   driver this really matters for is the rds driver, so that could
##   just be an argument passed there.  I think that's more general,
##   so might be better.
##
##   If that's the case, need some support for suppressing the mangling
##   temporarily.
##
## NOTE: This needs to be done via composition rather than inheritence
## because we only want to mangle each key once.  Otherwise we could
## make the first line of each method a string mangle.  Issue with
## that is we'd need to track whether or not we'd already digested a
## string and that requires a new class, etc, etc.  In short; this is
## not ideal, but should work.
##
## TODO: Store a set of key/hash mappings somewhere so we can look
## them up?  Or use base64 to *encode* the keys into something safe
## for filenames.  That might be better actually.  Leave it be for now.
storr_mangled_methods <- function(default_namespace) {
  public <- storr_methods(default_namespace)
  expr <- list(names=quote(if (is.null(names)) names else hash_string(names)),
               key=bquote(hash_string(key)))
  for (i in names(public)) {
    f <- public[[i]]
    fun_name <- call("$", call("$", quote(self), quote(storr)), as.name(i))
    fun_args <- lapply(names(formals(f)), as.symbol)
    t <- if (i %in% c("archive_export", "archive_export")) "names" else "key"
    j <- match(t, names(formals(f)))
    if (!is.na(j)) {
      fun_args[[j]] <- expr[[t]]
    }
    body(f) <- as.call(c(list(fun_name), fun_args))
    attr(f, "srcref") <- NULL
    public[[i]] <- f
  }
  public
}
