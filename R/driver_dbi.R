##' Object cache driver using the "DBI" package interface for storage.
##' This means that storr can work for any supported "DBI" driver
##' (e.g., SQLite, MySQL, Postgres, etc).  Because the DBI package
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
##' @title DBI storr driver
##' @param con A DBI connection object (see example)
##' @param tbl_data Name for the table that maps hashes to values
##' @param tbl_keys Name for the table that maps keys to hashes
##' @param binary Optional logical indicating if the values should be
##'   stored in binary.  If possible, this is both (potentially
##'   faster) and more accurate.  However, at present it is supported
##'   only under very recent DBI and RSQLite packages, and for no
##'   other DBI drivers, and is not actually any faster.  If not given
##'   (i.e., \code{NULL}), then binary storage will be used where
##'   possible when creating new tables, and where tables exist, we
##'   use whatever was used in the existing tables.
##' @param default_namespace Default namespace (see
##'   \code{\link{storr}}).
##' @export
##' @examples
##'
##' if (requireNamespace("RSQLite", quietly = TRUE)) {
##'   # Create an in-memory SQLite database and connection:
##'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
##'
##'   # From this create a storr:
##'   st <- storr_dbi(con, "tblData", "tblKeys")
##'
##'   # Set some data:
##'   st$set("foo", runif(10))
##'   st$list()
##'
##'   # And retrieve the data:
##'   st$get("foo")
##'
##'   # These are the data tables; treat these as read only
##'   DBI::dbListTables(con)
##'
##'   # With recent RSQLite you'll get binary storage here:
##'   st$driver$binary
##'
##'   # The entire storr part of the database can be removed using "destroy":
##'   st$destroy()
##'   DBI::dbListTables(con)
##' }
storr_dbi <- function(con, tbl_data, tbl_keys, binary=NULL,
                      default_namespace="objects") {
  storr(driver_dbi(con, tbl_data, tbl_keys, binary), default_namespace)
}

##' @rdname storr_dbi
##' @export
driver_dbi <- function(con, tbl_data, tbl_keys, binary=NULL) {
  .R6_driver_DBI$new(con, tbl_data, tbl_keys, binary)
}

.R6_driver_DBI <- R6::R6Class(
  "driver_DBI",

  public=list(
    con=NULL,
    tbl_data=NULL,
    tbl_keys=NULL,
    binary=NULL,
    supports_binary=FALSE,

    initialize=function(con, tbl_data, tbl_keys, binary=NULL) {
      loadNamespace("DBI")

      self$con <- con
      self$tbl_data <- tbl_data
      self$tbl_keys <- tbl_keys

      ## There's some logic in here:
      self$binary <- dbi_use_binary(con, tbl_data, binary)

      ## Initialise the tables.
      data_type <- if (self$binary) "BLOB" else "STRING"
      sql <- c(sprintf("CREATE TABLE if NOT EXISTS %s", tbl_data),
               "(hash STRING PRIMARY KEY NOT NULL,",
               sprintf("value %s NOT NULL)", data_type))
      DBI::dbGetQuery(self$con, paste(sql, collapse=" "))

      sql <- c(sprintf("CREATE TABLE IF NOT EXISTS %s", tbl_keys),
               "(namespace STRING NOT NULL,",
               "key STRING NOT NULL,",
               "hash STRING NOT NULL,",
               "PRIMARY KEY (namespace, key))")
      DBI::dbGetQuery(self$con, paste(sql, collapse=" "))
    },

    type=function() {
      paste0("DBI/", paste(class(self$con), collapse="/"))
    },

    ## Total destruction of the driver; delete all data stored in both
    ## tables, then delete our database connection to render the
    ## driver useless.
    destroy=function() {
      DBI::dbRemoveTable(self$con, self$tbl_data)
      DBI::dbRemoveTable(self$con, self$tbl_keys)
      self$con <- NULL
    },

    ## Return the hash value given a key/namespace pair
    get_hash=function(key, namespace) {
      sql <- sprintf('SELECT hash FROM "%s" WHERE namespace="%s" AND key="%s"',
                     self$tbl_keys, namespace, key)
      DBI::dbGetQuery(self$con, sql)[[1]]
    },

    ## Set the key/namespace pair to a hash
    set_hash=function(key, namespace, hash) {
      sql <- c(sprintf("INSERT OR REPLACE INTO %s", self$tbl_keys),
               sprintf('(namespace, key, hash) VALUES ("%s", "%s", "%s")',
                       namespace, key, hash))
      DBI::dbGetQuery(self$con, paste(sql, collapse=" "))
    },

    ## Return a (deserialised) R object, given a hash
    get_object=function(hash) {
      sql <- c(sprintf("SELECT value FROM %s", self$tbl_data),
               sprintf('WHERE hash = "%s"', hash))
      value <- DBI::dbGetQuery(self$con, paste(sql, collapse=" "))[[1]]
      if (self$binary) unserialize(value[[1]]) else unserialize_str(value[[1]])
    },

    ## TODO: Once supported, the new parameterised queries in recent
    ## DBIs make this much nicer, even when not using the binary
    ## interface.
    ##
    ## TODO: There is never a need to REPLACE the value here; we
    ## should offer to pass without ever sending the data.  Not sure
    ## how to do that without paying for a roundtrip though.  Check
    ## what support DBI has for not setting things, and see if that
    ## makes this any faster.
    ##
    ## TODO: Not sure, but it might be worth supporting the old-style
    ## SQLite interface here?  Seems a bad idea to start off with a
    ## kludge for backward compatibility when not needed though, and
    ## it does involve using a function that will be deprecated in the
    ## alarmingly near future.
    set_object=function(hash, value) {
      if (self$binary) {
        sql <- paste(sprintf("INSERT OR REPLACE INTO %s", self$tbl_data),
                     "(hash, value) VALUES (:hash, :value)")
        dat <- list(hash=hash, value=list(serialize(value, NULL)))
        DBI::dbGetQuery(self$con, sql, dat)
      } else {
        sql <- c(sprintf("INSERT OR REPLACE INTO %s", self$tbl_data),
                 sprintf('(hash, value) VALUES ("%s", "%s")',
                         hash, serialize_str(value)))
      }
    },

    ## Check if a key/namespace pair exists.
    exists_hash=function(key, namespace) {
      sql <- sprintf('SELECT 1 FROM %s WHERE namespace = "%s" AND key = "%s"',
                     self$tbl_keys, namespace, key)
      nrow(DBI::dbGetQuery(self$con, sql)) > 0L
    },
    ## Check if a hash exists
    exists_object=function(hash) {
      sql <- sprintf('SELECT 1 FROM %s WHERE hash = "%s"',
                     self$tbl_data, hash)
      nrow(DBI::dbGetQuery(self$con, sql)) > 0L
    },

    ## Delete a key.  Because of the requirement to return TRUE/FALSE on
    ## successful/unsuccessful key deletion this includes an exists_hash()
    ## step first.
    del_hash=function(key, namespace) {
      if (self$exists_hash(key, namespace)) {
        sql <- sprintf('DELETE FROM %s WHERE namespace = "%s" AND key = "%s"',
                       self$tbl_keys, namespace, key)
        DBI::dbGetQuery(self$con, sql)
        TRUE
      } else {
        FALSE
      }
    },
    ## Delete a hash
    del_object=function(hash) {
      if (self$exists_object(hash)) {
        sql <- sprintf('DELETE FROM %s WHERE hash = "%s"', self$tbl_data, hash)
        DBI::dbGetQuery(self$con, sql)
        TRUE
      } else {
        FALSE
      }
    },

    ## List hashes, namespaces and keys.  Because the SQLite driver seems to
    ## return numeric(0) if the result set is empty, we need as.character here.
    list_hashes=function() {
      sql <- sprintf("SELECT hash FROM %s", self$tbl_data)
      as.character(DBI::dbGetQuery(self$con, sql)[[1]])
    },
    list_namespaces=function() {
      sql <- sprintf("SELECT DISTINCT namespace FROM %s", self$tbl_keys)
      as.character(DBI::dbGetQuery(self$con, sql)[[1]])
    },
    list_keys=function(namespace) {
      sql <- sprintf('SELECT key FROM %s WHERE namespace="%s"',
                     self$tbl_keys, namespace)
      as.character(DBI::dbGetQuery(self$con, sql)[[1]])
    }
  ))

dbi_supports_binary <- function(con) {
  supports_binary <- FALSE
  ## Very little binary support exists; requires newfangled DBI and
  ## new RSQLite.  None of the other connection types supports binary
  ## serialisation.
  if (packageVersion("DBI") >= package_version("0.4.1")) {
    if (inherits(con, "SQLiteConnection") &&
        packageVersion("RSQLite") >= package_version("1.0.0")) {
      supports_binary <- TRUE
    }
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
      stop("Did not find 'data' column")
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
