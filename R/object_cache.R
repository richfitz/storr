##' Create an object cache
##' @title Object cache
##' @param driver A driver object
##' @export
object_cache <- function(driver) {
  .R6_object_cache$new(driver)
}

##' @importFrom R6 R6Class
.R6_object_cache <- R6::R6Class(
  "object_cache",

  public=list(
    driver=NULL,
    envir=NULL,

    initialize=function(driver) {
      self$driver <- driver
      self$envir  <- new.env(parent=emptyenv())
    },

    type=function(key) {
      if (!self$driver$exists_key(key)) {
        "none"
      } else if (self$is_list(key)) {
        "list"
      } else {
        "data"
      }
    },

    set=function(key, value, use_cache=TRUE) {
      hash <- hash_object(value)
      self$set_value(hash, value, use_cache)
      self$driver$set_key_hash(key, hash)
    },

    get=function(key, use_cache=TRUE) {
      if (self$is_list(key) && !self$driver$exists_key(key)) {
        value <- self$get_list(key, use_cache)
        self$set(key, value, use_cache)
        value
      } else {
        self$get_value(self$get_hash(key), use_cache)
      }
    },

    get_hash=function(key) {
      self$driver$get_hash(key)
    },

    del=function(key) {
      invisible(self$driver$del_key(key))
    },

    ## Totally naive garbage collection.  Should be replacable by the
    ## driver.  Note that this does *not* support lists yet!
    gc=function() {
      keys <- self$list()
      hashes_used <- vapply(keys, self$get_hash, character(1))
      hashes <- self$list_hashes()
      unused <- setdiff(hashes, hashes_used)
      for (h in unused) {
        self$driver$del_hash(h)
      }
      rm0(unused, self$envir)
      invisible(unused)
    },
    flush=function() {
      rm(list=ls(self$envir, all.names=TRUE), envir=self$envir)
    },

    get_value=function(hash, use_cache=TRUE) {
      if (use_cache && exists0(hash, self$envir)) {
        self$envir[[hash]]
      } else {
        value <- self$driver$get_value(hash)
        if (use_cache) {
          self$envir[[hash]] <- value
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

    list=function() {
      self$driver$list_keys()
    },
    list_hashes=function() {
      self$driver$list_hashes()
    },

    ## List support:
    is_list=function(key) {
      ## TODO: can we just assign functions up during initialization
      ## and then lock those bindings?  That'd be much nicer.
      ##   self$driver$is_list(key)
      !is.null(self$driver$is_list) && self$driver$is_list(key)
    },
    length_list=function(key) {
      self$driver$length_list(key)
    },

    set_list=function(key, value, use_cache=TRUE) {
      self$set_list_elements(key, value, NULL, use_cache)
      self$set(list_attr_name(key), attributes(value))
      self$set(key, value, use_cache)
    },
    ## "set list element" requires more care because we need to
    ## invalidate the current list and that requires tweaking get...
    get_list_element=function(key, i, use_cache=TRUE) {
      assert_scalar(i)
      hash <- self$driver$get_hash_list(key, i)
      self$get_value(hash, use_cache)
    },
    get_list_elements=function(key, i, use_cache=TRUE) {
      hash <- self$driver$get_hash_list(key, i)
      lapply(hash, self$get_value, use_cache)
    },
    ## NOTE: the order of arguments here (key, i, value) differs from
    ## the underlying driver (key, value, i) because of i being
    ## potentially optional.  That might change, but *this* order will
    ## remain the same.
    set_list_element=function(key, i, value, use_cache=TRUE) {
      assert_scalar(i)
      hash <- hash_object(value)
      self$set_value(hash, value, use_cache)
      self$driver$set_key_hash_list(key, hash, i)
      self$driver$del_key(key)
    },
    set_list_elements=function(key, values, i, use_cache=TRUE) {
      if (!is.null(i)) {
        assert_length(i, length(values))
      }
      hash_el <- vcapply(values, hash_object)
      for (j in seq_along(hash_el)) {
        self$set_value(hash_el[[i]], values[[i]], use_cache)
      }
      self$driver$set_key_hash_list(key, hash_el, i)
      self$driver$del_key(key)
    },
    get_list=function(key, use_cache=TRUE) {
      i <- seq_len(self$length_list(key))
      dat <- self$get_list_elements(key, i, use_cache)
      attributes(dat) <- self$get(list_attr_name(key))
      dat
    },

    ## TODO: for *all* the functions below:
    ##   (a) does this really belong as a class method?
    ##   (b) is the argument order correct?  `list` kind of wants to
    ##       be both first and last.
    ## To/from R environments (distinct from the environment driver)
    import=function(envir, list=NULL) {
      object_cache_copy(self, envir, list)
    },
    ## The logic here is taken from remake's object_store, which is
    ## useful as this is destined to replace that object.
    export=function(target, list=NULL) {
      object_cache_copy(target, self, list)
    },
    ## A simple convenience function, given that as.environment is not
    ## going to work (it's internal and the mode of an R6 class is
    ## already environment!)
    to_environment=function(parent=.GlobalEnv, list=NULL) {
      envir <- new.env(parent=parent)
      self$export(envir, list)
      envir
    }
  ))

## TODO: not a great mangling scheme - would be better to have an
## internal data annex I think; something that the drivers can provide
## however they want.  So then we'd say:
##   driver$set(key, value, namespace="list_info")
## with the default namespace be "objects" and with support for
## arbitrary namespaces.  I think that'll turn out generally useful
## actually.
##
## For now this will do.
list_attr_name <- function(key) {
  sprintf(".__list_info__%s", key)
}
