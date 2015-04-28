##' Create an object cache
##' @title Object cache
##' @param driver A driver object
##' @param default_namespace Default namespace to store objects in.
##' By default \code{"objects"} is used, but this might be useful to
##' have two diffent \code{storr} objects pointing at the same store,
##' but storing things in different namespaces.
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
    .R6_storr_mangled$new(driver, default_namespace)
  } else {
    .R6_storr$new(driver, default_namespace)
  }
}

##' @importFrom R6 R6Class
.R6_storr <- R6::R6Class(
  "storr",

  public=list(
    driver=NULL,
    envir=NULL,

    initialize=function(driver, default_namespace) {
      self$driver <- driver
      self$envir  <- new.env(parent=emptyenv())
      rewrite_namespace(self, default_namespace)
    },

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
      hash <- hash_object(value)
      self$set_value(hash, value, use_cache)
      self$driver$set_key_hash(key, hash, namespace)
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
      self$driver$get_hash(key, namespace)
    },

    del=function(key, namespace="objects") {
      ## TODO: This will report the wrong thing for lists being
      ## deleted much of the time.  Possibly drop due to YAGNI?
      if (self$driver$exists_list(key, namespace)) {
        self$driver$del_key(key, namespace="list_attr")
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

      lists <- self$driver$list_lists(ns)
      hashes_used2 <- unlist(lapply(lists, self$driver$get_hash_list,
                                    NULL, ns))
      hashes_used3 <- vcapply(lists, self$driver$get_hash,
                              "list_attr", USE.NAMES=FALSE)

      hashes_used <- unique(c(hashes_used1, hashes_used2, hashes_used3))

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
      self$set(key, attributes(value), "list_attr", use_cache)
      self$set(key, value, namespace, use_cache)
    },
    ## "set list element" requires more care because we need to
    ## invalidate the current list and that requires tweaking get...
    get_list_element=function(key, i, namespace="objects", use_cache=TRUE) {
      assert_scalar(i)
      hash <- self$driver$get_hash_list(key, i, namespace)
      self$get_value(hash, use_cache)
    },
    get_list_elements=function(key, i, namespace="objects", use_cache=TRUE) {
      hash <- self$driver$get_hash_list(key, i, namespace)
      lapply(hash, self$get_value, use_cache)
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
      attributes(value) <- self$get(key, namespace="list_attr")
      value
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
  ))

## This is another version that will arrange to mangle key *names* to
## their hash.  This results in a store where we cannot determine the
## keys for what is stored in it!
##
## This is pretty evil code generation.  There may be a better way,
## but this avoids a lot of nasty repetition.
storr_mangled_methods <- function() {
  self <- NULL # avoid false positive NOTE
  ret <- list(storr=NULL,
              initialize=function(...) {
                self$storr <- storr(...)
                rewrite_namespace(self, formals(self$storr$get)$namespace)
              })

  public <- .R6_storr$public_methods
  for (m in setdiff(names(public), "initialize")) {
    f <- public[[m]]
    fun_name <- call("$", call("$", quote(self), quote(storr)), as.name(m))
    fun_args <- lapply(names(formals(f)), as.symbol)
    if ("key" %in% names(formals(f))) {
      fun_args[[match("key", names(formals(f)))]] <-
        call("hash_string", quote(key))
    }
    body(f) <- as.call(c(list(fun_name), fun_args))
    ret[[m]] <- f
  }

  ret
}

.R6_storr_mangled <- R6::R6Class(
  "storr_mangled",
  public=storr_mangled_methods())

## This trades off (little) initialization speed for (little)
## execution speed, but also provides a clearer UI because the
## actual default namespace will be the function argument.
##
## This triggers a NOTE in R CMD check but should be OK here given
## we're hitting things that we control directly.
rewrite_namespace <- function(self, default_namespace) {
  if (default_namespace != "objects") {
    for (m in ls(self)) {
      if ("namespace" %in% names(formals(self[[m]]))) {
        modify_defaults_R6(self, m, "namespace", default_namespace)
      }
    }
  }
}
