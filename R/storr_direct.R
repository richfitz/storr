R6_storr_direct <- R6::R6Class(
  "storr_direct",
  public = list(
    driver = NULL,
    default_namespace = NULL,
    traits = NULL,

    initialize = function(driver, default_namespace) {
      if (!isTRUE(driver$is_direct)) {
        stop("driver is not in 'direct' mode")
      }
      self$driver <- driver
      self$default_namespace <- default_namespace
      self$traits <- storr_traits(driver$traits)
    },

    destroy = function() {
      self$driver$destroy()
      self$driver <- NULL
    },

    set = function(key, value, namespace = self$default_namespace) {
      self$driver$set_hash(key, namespace, value)
    },

    mset = function(key, value, namespace = self$default_namespace) {
      n <- check_length(key, namespace)
      assert_length(value, n)
      if (is.null(self$driver$mset_hash)) {
        for (i in seq_len(n)) {
          key <- rep_len(key, n)
          namespace <- rep_len(namespace, n)
          self$driver$set_hash(key[[i]], namespace[[i]], value[[i]])
        }
      } else {
        self$driver$mset_hash(key, namespace, value)
      }
      invisible(n)
    },

    get = function(key, namespace = self$default_namespace) {
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

    mget = function(key, namespace = self$default_namespace) {
      n <- check_length(key, namespace)
      ## TODO: this does not do the flagging as to whether things are
      ## missing or stored as NULL, yet.
      if (is.null(self$driver$mget_hash)) {
        m <- cbind(key, namespace)
        lapply(seq_len(n), function(i)
          tryCatch(self$driver$get_hash(m[i, 1L], m[i, 2L]),
                   KeyError = function(e) NULL))
      } else {
        self$driver$mget_hash(key, namespace)
      }
    },

    del = function(key, namespace = self$default_namespace) {
      n <- check_length(key, namespace)
      invisible(self$driver$del_hash(key, namespace))
    },

    exists = function(key, namespace = self$default_namespace) {
      self$driver$exists_hash(key, namespace)
    },

    list = function(namespace = self$default_namespace) {
      sort(self$driver$list_keys(namespace))
    },

    list_namespaces = function() {
      sort(self$driver$list_namespaces())
    }

    ## TODO:
    ##
    ## import(src, list, namespace, skip_missing)
    ## export(dest, list, namespace, skip_missing)
    ## archive_export(path, names, namespace)
    ## archive_import(path, names, namespace)
  ))
