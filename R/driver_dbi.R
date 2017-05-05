##' Object cache driver using the "DBI" package interface for storage.
##' This means that storr can work for any supported "DBI" driver
##' (though practically this works only for SQLite and Postgres until
##' some MySQL dialect translation is done).  To connect, you must
##' provide the \emph{driver} object (e.g., \code{RSQLite::SQLite()},
##' \code{RPostgreSQL::PostgreSQL()}, \code{RPostgres::Postgres()} as
##' the first argument,
##'
##' Because the DBI package
##' specifies a uniform interface for the using DBI compliant
##' databases, you need only to provide a connection object.  storr
##' does not do anything to help create the connection object itself.
##'
##' The DBI storr driver works by using two tables; one mapping keys
##' to hashes, and one mapping hashes to values.  Two table names need
##' to be provided here; they must be different and they should be
##' treated as opaque (don't use them for anything else - reading or
##' writing).  Apart from that the names do not matter.
##'
##' Because of treatment of binary data by the underlying DBI drivers,
##' binary serialistion is not any faster (and might be slightly
##' slower than) string serialisation, in contrast with my experience
##' with other backends.
##'
##' Be aware that \code{$destroy()} will close the connection to the
##' database.
##'
##' @title DBI storr driver
##'
##' @param tbl_data Name for the table that maps hashes to values
##'
##' @param tbl_keys Name for the table that maps keys to hashes
##'
##' @param con Either A DBI connection or a DBI driver (see example)
##'
##' @param args Arguments to pass, along with the driver, to
##'   \code{DBI::dbConnect} if \code{con} is a driver.
##'
##' @param binary Optional logical indicating if the values should be
##'   stored in binary.  If possible, this is both (potentially
##'   faster) and more accurate.  However, at present it is supported
##'   only under very recent DBI and RSQLite packages, and for no
##'   other DBI drivers, and is not actually any faster.  If not given
##'   (i.e., \code{NULL}), then binary storage will be used where
##'   possible when creating new tables, and where tables exist, we
##'   use whatever was used in the existing tables.
##'
##' @param hash_algorithm Name of the hash algorithm to use.  Possible
##'   values are "md5", "sha1", and others supported by
##'   \code{\link{digest}}.  If not given, then we will default to
##'   "md5".
##'
##' @param default_namespace Default namespace (see
##'   \code{\link{storr}}).
##'
##' @export
##' @examples
##' if (requireNamespace("RSQLite", quietly = TRUE)) {
##'   st <- storr::storr_dbi("tblData", "tblKeys", RSQLite::SQLite(),
##'                          ":memory:")
##'
##'   # Set some data:
##'   st$set("foo", runif(10))
##'   st$list()
##'
##'   # And retrieve the data:
##'   st$get("foo")
##'
##'   # These are the data tables; treat these as read only
##'   DBI::dbListTables(st$driver$con)
##'
##'   # With recent RSQLite you'll get binary storage here:
##'   st$driver$binary
##'
##'   # The entire storr part of the database can be removed using
##'   # "destroy"; this will also close the connection to the database
##'   st$destroy()
##'
##'   # If you have a connection you want to reuse (which will the the
##'   # case if you are using an in-memory SQLite database for
##'   # multiple things within an application) it may be useful to
##'   # pass the connection object instead of the driver:
##'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
##'   st <- storr::storr_dbi("tblData", "tblKeys", con)
##'   st$set("foo", runif(10))
##'
##'   # You can then connect a different storr to the same underlying
##'   # storage
##'   st2 <- storr::storr_dbi("tblData", "tblKeys", con)
##'   st2$get("foo")
##' }
storr_dbi <- function(tbl_data, tbl_keys, con, args = NULL, binary = NULL,
                      hash_algorithm = NULL,
                      default_namespace = "objects") {
  storr(driver_dbi(tbl_data, tbl_keys, con, args, binary, hash_algorithm),
        default_namespace)
}

##' @rdname storr_dbi
##' @export
driver_dbi <- function(tbl_data, tbl_keys, con, args = NULL, binary = NULL,
                       hash_algorithm = NULL) {
  R6_driver_DBI$new(tbl_data, tbl_keys, con, args, binary, hash_algorithm)
}

R6_driver_DBI <- R6::R6Class(
  "driver_DBI",

  public = list(
    con = NULL,
    con_connect = NULL,
    con_type = NULL,
    tbl_data = NULL,
    tbl_keys = NULL,
    traits = NULL,
    binary = NULL,
    hash_algorithm = NULL,
    sql = NULL,

    initialize = function(tbl_data, tbl_keys, con, args, binary = NULL,
                          hash_algorithm = NULL) {
      loadNamespace("DBI")

      if (inherits(con, driver_classes())) {
        self$con_connect <- dbi_connection_factory(NULL, args)
        self$con <- con
      } else {
        self$con_connect <- dbi_connection_factory(con, args)
        self$con <- self$con_connect()
      }
      self$con_type <- class(self$con)

      self$tbl_data <- tbl_data
      self$tbl_keys <- tbl_keys

      ## There's some logic in here:
      self$binary <- dbi_use_binary(self$con, tbl_data, binary)

      ## TODO: Is it possible to support throw_missing?
      self$traits <- list(accept = if (self$binary) "raw" else "string")
      self$sql <- driver_dbi_sql_compat(self$con, tbl_data, tbl_keys)

      ## Initialise the tables.
      if (!DBI::dbExistsTable(self$con, tbl_data)) {
        data_type <- if (self$binary) "BLOB" else "TEXT"
        sql <- c(sprintf("CREATE TABLE %s", dquote(tbl_data)),
                 "(hash TEXT PRIMARY KEY NOT NULL,",
                 sprintf("value %s NOT NULL)", data_type))
        DBI::dbExecute(self$con, paste(sql, collapse = " "))
      }

      if (!DBI::dbExistsTable(self$con, tbl_keys)) {
        ## TODO: hash here could has a proper length (CHAR(X))
        ## which might give better performance.  Probably worth
        ## checking at some point.
        ##
        ## Also, TEXT is not standard, though widely supported
        sql <- c(sprintf("CREATE TABLE %s", dquote(tbl_keys)),
                 "(namespace TEXT NOT NULL,",
                 "key TEXT NOT NULL,",
                 "hash TEXT NOT NULL,",
                 "PRIMARY KEY (namespace, key))")
        DBI::dbExecute(self$con, paste(sql, collapse = " "))
      }

      ## TODO: This will work just fine unless we do set the hash to
      ## have a very particular length.
      ##
      ## TODO: See dbi_use_binary for an alternative approach for
      ## dealing with configuration; I don't think we can really use
      ## that here though?
      if (self$exists_object(STORR_DBI_CONFIG_HASH)) {
        config <- self$get_object(STORR_DBI_CONFIG_HASH)
      } else {
        config <- list()
      }
      if (!is.null(hash_algorithm)) {
        assert_scalar_character(hash_algorithm)
      }
      if (is.null(config$hash_algorithm)) {
        config$hash_algorithm <- hash_algorithm %||% "md5"
        config_ser <- make_serialize_object(FALSE, !self$binary)(config)
        ## Need to arrange to serialize the object here, because
        ## ordinarily storr will take care of that for us.
        hh <- self$set_object(STORR_DBI_CONFIG_HASH, config_ser)
      } else {
        if (is.null(hash_algorithm)) {
          hash_algorithm <- config$hash_algorithm
        } else {
          if (hash_algorithm != config$hash_algorithm) {
            stop(ConfigError("hash_algorithm", config$hash_algorithm,
                             hash_algorithm))
          }
        }
      }
      self$hash_algorithm <- config$hash_algorithm
    },

    type = function() {
      ## TODO: now that we have a dialect detection thing it might be
      ## good to list that here.
      paste0("DBI/", paste(self$con_type, collapse = "/"))
    },

    ## Total destruction of the driver; delete all data stored in both
    ## tables, then delete our database connection to render the
    ## driver useless.
    destroy = function() {
      DBI::dbRemoveTable(self$con, self$tbl_data)
      DBI::dbRemoveTable(self$con, self$tbl_keys)
      DBI::dbDisconnect(self$con)
      self$con <- NULL
    },

    ## Return the hash value given a key/namespace pair
    get_hash = function(key, namespace) {
      DBI::dbGetQuery(self$con, self$sql$get_hash, list(namespace, key))[[1]]
    },

    mget_hash = function(key, namespace) {
      tmp <- driver_dbi_mkey_prepare(key, namespace, self$sql$placeholder)
      if (is.null(tmp)) {
        return(character(0))
      }
      sql <- sprintf(self$sql$mget_hash, tmp$where)
      dat <- DBI::dbGetQuery(self$con, sql, tmp$values)
      as.character(dat$hash)[match(tmp$requested, tmp$returned(dat))]
    },

    ## Set the key/namespace pair to a hash
    set_hash = function(key, namespace, hash) {
      dat <- list(namespace, key, hash)
      DBI::dbExecute(self$con, self$sql$set_hash, dat)
    },

    mset_hash = function(key, namespace, hash) {
      ## NOTE: hash is guaranteed to be correct (and canonical) length
      ## for key/namespace
      if (length(hash) == 0L) {
        return()
      }
      dat <- rbind(namespace, key, hash, deparse.level = 0)
      keep <- !duplicated(t(dat[1:2, , drop = FALSE]), fromLast = TRUE)
      if (all(keep)) {
        dat <- as.list(dat)
      } else {
        dat <- as.list(dat[, keep, drop = FALSE])
      }
      p <- group_placeholders(self$sql$placeholder, 3L, sum(keep))
      DBI::dbExecute(self$con, sprintf(self$sql$mset_hash, p), dat)
    },

    ## Return a (deserialized) R object, given a hash
    get_object = function(hash) {
      value <- DBI::dbGetQuery(self$con, self$sql$get_object, list(hash))[[1L]]
      unserialize_safe(value[[1L]])
    },

    mget_object = function(hash) {
      if (length(hash) == 0L){
        return(list())
      }
      p <- paste(sprintf(self$sql$placeholder, seq_along(hash)),
                 collapse = ", ")
      value <- DBI::dbGetQuery(self$con, sprintf(self$sql$mget_object, p), hash)
      i <- match(hash, value$hash)
      j <- !is.na(i)
      if (all(j)) {
        ret <- lapply(value$value[i], unserialize_safe)
      } else {
        ret <- vector("list", length(hash))
        if (any(j)) {
          ret[j] <- lapply(value$value[i][j], unserialize_safe)
        }
      }
      ret
    },

    set_object = function(hash, value) {
      ## TODO: Could tidy up the serialization inteface (I'm sure I
      ## have helpers for that somewhere).  Though OTOH if we accept
      ## binary we can skip the serialisation here anyway with
      ## appropriate setting of traits.
      dat <- list(hash, if (self$binary) list(value) else value)
      DBI::dbExecute(self$con, self$sql$set_object, dat)
    },

    mset_object = function(hash, value) {
      if (length(value) == 0L) {
        return()
      }
      dat <- vector("list", length(value) * 2L)
      j <- seq.int(1L, by = 2L, length.out = length(value))
      dat[j] <- hash
      if (self$binary) {
        dat[j + 1L] <- lapply(value, list)
      } else {
        dat[j + 1L] <- value
      }

      p <- group_placeholders(self$sql$placeholder, 2L, length(value))
      DBI::dbExecute(self$con, sprintf(self$sql$mset_object, p), dat)
    },

    ## Check if a key/namespace pair exists.
    exists_hash = function(key, namespace) {
      nk <- join_key_namespace(key, namespace)
      if (nk$n == 1L) {
        sql <- sprintf(
          "SELECT 1 FROM \"%s\" WHERE namespace = '%s' AND key = '%s'",
          self$tbl_keys, namespace, key)
        nrow(DBI::dbGetQuery(self$con, sql)) > 0L
      } else if (nk$n == 0L) {
        logical(0)
      } else {
        tmp <-
          driver_dbi_mkey_prepare(nk$key, nk$namespace, self$sql$placeholder)
        sql <- sprintf('SELECT key, namespace FROM "%s" WHERE %s',
                       self$tbl_keys, tmp$where)
        res <- tmp$returned(DBI::dbGetQuery(self$con, sql, tmp$values))
        tmp$requested %in% res
      }
    },

    ## Check if a hash exists
    exists_object = function(hash) {
      if (length(hash) == 0L) {
        logical(0)
      } else if (length(hash) == 1L) {
        sql <- sprintf("SELECT 1 FROM \"%s\" WHERE hash = '%s'",
                       self$tbl_data, hash)
        nrow(DBI::dbGetQuery(self$con, sql)) > 0L
      } else {
        sql <- sprintf("SELECT hash FROM \"%s\" WHERE hash IN (%s)",
                       self$tbl_data, paste(squote(hash), collapse = ", "))
        hash %in% DBI::dbGetQuery(self$con, sql)$hash
      }
    },

    ## Delete a key.  Because of the requirement to return TRUE/FALSE
    ## on successful/unsuccessful key deletion this includes an
    ## exists_hash() step first, otherwise we need to some very ugly
    ## (and probably non-portable) queries to return booleans as we
    ## delete things.
    del_hash = function(key, namespace) {
      exists <- self$exists_hash(key, namespace)
      if (any(exists)) {
        ## NOTE: at least one key guaranteed here:
        tmp <- driver_dbi_mkey_prepare(key, namespace, self$sql$placeholder)
        sql <- sprintf('DELETE FROM "%s" WHERE %s', self$tbl_keys, tmp$where)
        DBI::dbExecute(self$con, sql, tmp$values)
      }
      exists
    },

    ## Delete a hash
    del_object = function(hash) {
      exists <- self$exists_object(hash)
      if (any(exists)) {
        hash_del <- hash[exists]
        if (length(hash_del) == 1L) {
          sql <- sprintf("DELETE FROM \"%s\" WHERE hash = '%s'",
                         self$tbl_data, hash_del)
        } else {
          sql <- sprintf("DELETE FROM \"%s\" WHERE hash IN (%s)",
                         self$tbl_data,
                         paste(squote(hash_del), collapse = ", "))
        }
        DBI::dbExecute(self$con, sql)
      }
      exists
    },

    ## List hashes, namespaces and keys.  Because the SQLite driver seems to
    ## return numeric(0) if the result set is empty, we need as.character here.
    list_hashes = function() {
      sql <- sprintf('SELECT hash FROM "%s"', self$tbl_data)
      setdiff(as.character(DBI::dbGetQuery(self$con, sql)[[1]]),
              STORR_DBI_CONFIG_HASH)
    },

    list_namespaces = function() {
      sql <- sprintf('SELECT DISTINCT namespace FROM "%s"', self$tbl_keys)
      res <- DBI::dbGetQuery(self$con, sql)
      if (nrow(res) > 0L) res[[1L]] else character(0)
    },

    list_keys = function(namespace) {
      sql <- sprintf("SELECT key FROM \"%s\" WHERE namespace = '%s'",
                     self$tbl_keys, namespace)
      res <- DBI::dbGetQuery(self$con, sql)
      if (nrow(res) > 0L) res[[1L]] else character(0)
    },

    disconnect = function() {
      DBI::dbDisconnect(self$con)
      self$con <- NULL
    },

    reconnect = function() {
      self$con <- self$con_connect()
    }
  ))

dbi_supports_binary <- function(con) {
  ## Very little binary support exists; requires newfangled DBI and
  ## new RSQLite.  None of the other connection types supports binary
  ## serialisation, though hopefully it will be supported in new
  ## versions of RPostgres.  Minimum versions that support binary are
  ## enforced via the DESCRIPTION.
  supports_binary <- FALSE
  if (inherits(con, "SQLiteConnection")) {
    supports_binary <- TRUE
  }
  supports_binary
}

dbi_use_binary <- function(con, tbl_data, binary) {
  supports_binary <- dbi_supports_binary(con)
  if (!is.null(binary)) {
    assert_scalar_logical(binary)
    binary <- as.vector(binary) # drop names, attributes
  }
  if (isTRUE(as.vector(binary)) && !supports_binary) {
    stop("Binary storage requested but storage driver does not support it")
  }

  if (DBI::dbExistsTable(con, tbl_data)) {
    sql <- sprintf("SELECT * from %s LIMIT 0", tbl_data)
    rs <- DBI::dbSendQuery(con, sql)
    on.exit(DBI::dbClearResult(rs))
    res <- DBI::dbColumnInfo(rs)
    t <- res$type[match("value", res$name)]
    if (is.na(t)) {
      stop("Did not find 'value' column")
    } else {
      binary_found <- t == "list" # small assumption here that this means BLOB
    }
    if (!is.null(binary) && binary_found != binary) {
      stop(sprintf("Requested %s storage conflicts with existing %s storage",
                   if (binary) "binary" else "non-binary",
                   if (binary_found) "binary" else "non-binary"))
    }
    binary_found
  } else {
    if (is.null(binary)) supports_binary else binary
  }
}

## 15 'f's - no hash algo has this length
STORR_DBI_CONFIG_HASH <- paste(rep("f", 15), collapse = "")

## This is going to hold prepared queries for the core bits of the
## SQL; these vary fairly wildly by dialect, and some care is needed
## here.
##
## Drivers to get working:
##
##   * sqlite (SQLiteConnection)
##   * postgres (PqConnection - also check the one on CRAN?)
##   * mysql (unknown)
##   * crate (varies though)
##
## With ODBC drivers getting the underlying dialect is what is important.
##
## THis might be a bit fragile but I don't really see how to
## generalise it that much (short of something huge like Python's
## sqlalchemy).
driver_dbi_sql_compat <- function(con, tbl_data, tbl_keys) {
  j <- function(...) {
    paste(c(...), collapse = " ")
  }
  dialect <- driver_dbi_dialect(con)
  if (dialect == "sqlite") {
    ret <- list(
      ## Scalar:
      get_hash = j("SELECT hash FROM", dquote(tbl_keys),
                   "WHERE namespace = ? AND KEY = ?"),
      set_hash = j("INSERT OR REPLACE INTO", dquote(tbl_keys),
                   "(namespace, key, hash) VALUES (?, ?, ?)"),
      get_object = j("SELECT value FROM", dquote(tbl_data),
                     "WHERE hash = ?"),
      set_object = j("INSERT OR REPLACE INTO", dquote(tbl_data),
                     "(hash, value) VALUES (?, ?)"),
      ## Vector:
      mget_hash = j("SELECT * FROM", dquote(tbl_keys),
                    "WHERE %s"),
      mset_hash = j("INSERT OR REPLACE INTO", dquote(tbl_keys),
                    "(namespace, key, hash) VALUES %s"),
      mget_object = j("SELECT * FROM", dquote(tbl_data),
                      "WHERE hash IN (%s)"),
      mset_object = j("INSERT OR REPLACE INTO", dquote(tbl_data),
                      "(hash, value) VALUES %s"),
      placeholder = "?"
    )
  } else if (dialect == "postgresql") {
    ## Before 0.9.5 there was no simple way of implementing the
    ## "INSERT OR REPLACE INTO" pattern (via INSERT INTO ... ON
    ## CONFLICT REPLACE" and I'm just going to require a recent
    ## version for simplicity.
    v <- pg_server_version(con)
    if (v < numeric_version("0.9.5")) {
      stop(sprintf(
        "Version %s of postgresql server is not supported (need >= 0.9.5)", v))
    }
    ret <- list(
      get_hash = j("SELECT hash FROM", dquote(tbl_keys),
                   "WHERE namespace = $1 AND KEY = $2"),
      set_hash = j("INSERT INTO", dquote(tbl_keys),
                   "(namespace, key, hash) VALUES ($1, $2, $3)",
                   "ON CONFLICT (namespace, key) DO UPDATE",
                   "SET hash = excluded.hash"),
      get_object = j("SELECT value FROM", dquote(tbl_data),
                     "WHERE hash = $1"),
      set_object = j("INSERT INTO", dquote(tbl_data),
                     "(hash, value) VALUES ($1, $2)",
                     "ON CONFLICT (hash) DO NOTHING"),
      ## Vector:
      mget_hash = j("SELECT * FROM", dquote(tbl_keys),
                    "WHERE %s"),
      mset_hash = j("INSERT INTO", dquote(tbl_keys),
                    "(namespace, key, hash) VALUES %s",
                    "ON CONFLICT (namespace, key) DO UPDATE",
                    "SET hash = excluded.hash"),
      mget_object = j("SELECT * FROM", dquote(tbl_data),
                      "WHERE hash IN (%s)"),
      mset_object = j("INSERT INTO", dquote(tbl_data),
                      "(hash, value) VALUES %s",
                      "ON CONFLICT (hash) DO NOTHING"),
      placeholder = "$%d"
    )
  } else {
    stop("Unsupported SQL dialect ", dialect, " [storr bug]") # nocov
  }
  ret
}

driver_dbi_mkey_prepare <- function(key, namespace, placeholder) {
  nk <- join_key_namespace(key, namespace)
  if (nk$n == 0L) {
    return(NULL)
  }

  ## NOTE: this is the same strategy as used by unique.matrix; it's
  ## not pretty but given the constraints of sql databases is probably
  ## pretty good here?
  requested <- paste(nk$key, nk$namespace, sep = "\r")
  keep <- !duplicated(requested)

  key_uniq <- nk$key[keep]
  ns_uniq <- nk$namespace[keep]
  n_key <- length(unique(key_uniq))
  n_namespace <- length(unique(ns_uniq))

  ## TODO: don't use IN for the single cases, use '='; this requires
  ## some treatment in the final clause too, which repeats this whole
  ## thing.
  if (n_namespace == 1) {
    values <- c(ns_uniq[1L], key_uniq)
    p <- sprintf(placeholder, seq_along(values))
    where <- sprintf("namespace = %s AND key IN (%s)",
                     p[[1L]], paste(p[-1L], collapse = ", "))
  } else if (n_key == 1) {
    values <- c(key_uniq[1L], ns_uniq)
    p <- sprintf(placeholder, seq_along(values))
    where <- sprintf("key = %s AND namespace IN (%s)",
                     p[[1L]], paste(p[-1L], collapse = ", "))
  } else {
    ## There's a big hassle here of getting things nailed into place
    i <- unname(split(seq_along(ns_uniq), ns_uniq))
    len <- lengths(i)

    ## A little abuse of R semantics here to interleave namespaces with values:
    values <- unlist(rbind(lapply(i, function(j) ns_uniq[[j[[1L]]]]),
                           lapply(i, function(j) key_uniq[j])))

    p <- sprintf(placeholder, seq_along(values))
    p <- unname(split(p, rep(seq_along(len), len + 1)))
    where <- paste(vcapply(p, function(x)
      sprintf("namespace = %s AND key IN (%s)",
              x[[1L]], paste(x[-1L], collapse = ", "))), collapse = " OR ")
  }
  list(requested = requested,
       returned = function(dat) paste(dat$key, dat$namespace, sep = "\r"),
       where = where,
       n = nk$n,
       values = values)
}

driver_dbi_dialect <- function(con) {
  if (inherits(con, "SQLiteConnection")) {
    "sqlite"
  } else if (inherits(con, c("PqConnection", "PostgreSQLConnection"))) {
    "postgresql"
  } else {
    stop("Unsupported SQL driver of class ", paste(class(con), collapse = "/"),
         call. = FALSE)
  }
}

driver_classes <- function() {
  c("SQLiteConnection",
    "PqConnection", "PostgreSQLConnection")
}

group_placeholders <- function(placeholder, n, times) {
  p <- matrix(sprintf(placeholder, seq_len(n * times)), n)
  paste(sprintf("(%s)", apply(p, 2, paste, collapse = ", ")), collapse = ", ")
}

pg_server_version <- function(con) {
  numeric_version(DBI::dbGetQuery(con, "show server_version")[[1L]])
}

dbi_connection_factory <- function(drv, args) {
  if (is.null(drv)) {
    if (!is.null(args)) {
      stop("Cannot specify arguments when passing a connection object")
    }
    function() {
      stop("Cannot reconnect to this database")
    }
  } else {
    args <- c(list(drv), args)
    function() {
      do.call(DBI::dbConnect, args)
    }
  }
}
