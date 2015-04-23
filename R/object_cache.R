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

    set=function(key, value, cache=TRUE) {
      hash <- hash_object(value)
      if (!self$driver$exists_hash(hash)) {
        self$driver$set_hash_value(hash, value)
      }
      self$driver$set_key_hash(key, hash)
      if (cache && !exists0(hash, self$envir)) {
        assign(hash, value, self$envir)
      }
    },

    get=function(key, use_cache=TRUE) {
      self$get_value(self$get_hash(key), use_cache)
    },

    get_hash=function(key) {
      self$driver$get_hash(key)
    },

    del=function(key) {
      invisible(self$driver$del_key(key))
    },

    ## Totally naive garbage collection.  Should be replacable by the
    ## driver.
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

    get_value=function(hash, use_cache=TRUE) {
      if (use_cache && exists0(hash, self$envir)) {
        self$envir[[hash]]
      } else {
        self$driver$get_value(hash)
      }
    },

    list=function() {
      self$driver$list_keys()
    },
    list_hashes=function() {
      self$driver$list_hashes()
    }
  ))
