## ---
## title: "storr drivers"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{storr drivers}
##   %\VignetteEngine{knitr::rmarkdown}
##   \usepackage[utf8]{inputenc}
## ---

## Requirements for storr drivers.

## The idea here is that you implement a handful of methods and the
## package will construct a common interface around them.  There are
## built-in tests in the package to ensure that the driver behaves
## correctly, and infrastructure to help with running those tests.

## To demonstrate we'll write a wrapper around RSQLite to store data
## (NOTE: this has since been rolled into the package as an actual
## driver `driver_dbi` but I'm leaving it here for posterity).

## # How it works and what we need

## First, consider the `get` method in storr.  With a driver `dr`,
## storr retrieves values by running (approximately):

## ```r
## dr$exists_hash(key, namespace)
## hash <- dr$get_hash(key, namespace)
## dr$exists_object(hash)
## dr$get_object(hash)
## ```

## which:
##
## 1. checks that a key exists (keys being defined by a combination of
## key and namespace)
## 2. retrieves the object hash stored against that key
## 3. checks that the hash is actually present in the database
## 4. retrieves the object stored against the hash
##
## hashes are stored as strings, while objects are *serialized R
## objects*, usually stored in binary.  The driver is responsible for
## serialization/deserialization as that will depend on the properties
## of the driver.
##
## storr will take care of throwing appropriate errors if the object
## is not found (which requires the calls to `exists_hash` and
## `exists_object`).

## `set` works in a similar way:
##
## ```r
## hash <- storr:::hash_object(hash)
## if (!dr$exists_object(hash)) {
##   dr$set_object(hash, value)
## }
## dr$set_hash(key, namespace, hash)
## ```
##
## The important part here is that storr will avoid setting the object
## if it can be avoided (i.e., if the hash is present in the database
## then the object has already been stored -- because saving the
## actual data is likely to be the slowest part it's worth avoiding).
##
## 1. if the hash is not present, save the (serialized) object against
## the hash.
## 2. store the hash against the key and namespace.

## The full list of functions needed follows the next section.

## ## A digression: key/value stores and SQL

## SQL databases are probably not going to be a great place to store
## key/value data (especially very large objects) and this section is
## not meant to be normative.  Instead, this is one possible route
## that could be taken.  Recent version of postgresql include
## interfaces that support first class key/value (`hstore`) which
## would be prefereable to this.

## Start with a SQLite connection (similar things can be done with
## other DBI drivers but at present this uses one SQLite-only function
## below):
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

## This will create a table into which we can put key/value pairs.
table <- "mydata"
sql <- c(sprintf("CREATE TABLE IF NOT EXISTS %s", table),
         "(name string PRIMARY KEY,",
         "value blob)")
DBI::dbExecute(con, paste(sql, collapse = " "))

## Then take an object, serialize it, and stuff it into the blob, then
## insert that into the table.  This is the part that varies between
## versions.

## For both versions, we'll store the *value* of `mtcars` with the
## *name* `"mtcars"`:
value <- mtcars
name <- "mtcars"
sql <- sprintf("INSERT into %s (name, value) values (:name, :value)", table)

dat <- list(name = name, value = list(serialize(value, NULL)))
DBI::dbExecute(con, sql, dat)

## The pattern here is to use `dbExecute` to create and execute the
## query, injecting the raw byte sequence of the serialized object
## into the `value` column.  This is currently supported only for
## RSQLite in the current release, but will probably be supported by
## other DBI-compatible packages over time.

## We can retrieve the data by name:
sql <- sprintf('SELECT value FROM %s WHERE name == "%s"', table, name)
x <- unserialize(DBI::dbGetQuery(con, sql)[[1]][[1]])
identical(x, value)

## ## Implementing the storr driver

## For consistency with other `storr` functions, even though the
## driver is an R6 object, we construct it using a plain R function
## (not the `$new()` method of an R6 generator.  This is because the
## driver *could* be implemented as a function that generates a list
## of closures, a reference class, or some other approach.

## There are quite a few required functions to implement.  Below, the
## value in parentheses after the function signature is the expected
## return type.

## * `type()` (returns character): Takes no argument and returns a
##   string with the "type" of the driver.  This can be any
##   identifier.
## * `destroy()` (returns NULL): Takes no argument and destroys all
##   data associated with the driver.  Best to also leave the driver
##   in a state where it can't be used, though this is not enforced.

## * `set_hash(key, namespace, hash)` (returns NULL): Given strings
##   for `key`, `namespace` and `hash`, set the `hash` against the
##   `key`/`namespace` pair.
## * `get_hash(key, namespace)` (returns character): Given strings for
##   `key`, and `namespace`, return the hash of the object.

## * `set_object(hash, value)` (returns NULL): Given a string `hash`
##   and an arbitrary object `value`, store the object against the
##   hash.  Serialization will likely be needed here (e.g.,
##   `serialize(value, NULL)`).
## * `get_object(hash)` (returns object): Given a string `hash` return
##   the R object stored against it.  Deserialization will likely be
##   needed here (e.g., `unserialize(dat)`)

## * `exists_hash(key, namespace)` (returns logical): Given vectors of
##   strings for `key` and `namespace` return a vector with `TRUE` if
##   there is a hash stored against the key/namespace pair, `FALSE`
##   otherwise.
## * `exists_object(hash)` (returns logical): Given a vector of
##   strings for `hash`, return a vector with `TRUE` if there is an
##   object stored against the hash.

## * `del_hash(key, namespace)` (returns logical): Given strings for
##   `key` and `namespace`, delete this key if it exists.  Return
##   `TRUE` if the key existed, `FALSE` otherwise.
## * `del_object(hash)` (returns logical): Given a string for `hash`
##   the object if it exists.  Return `TRUE` if the hash existed,
##   `FALSE` otherwise.

## * `list_hashes()` (returns character vector): Return a character
##   vector of all known hashes.
##
## * `list_namespaces()` (returns character vector): Return a
##   character vector of all known namespaces.
## * `list_keys(namespace)` (returns character vector): Given a string
##   `namespace`, return a character vector of all known keys within
##   the namespace.

## The arguments to the constructor will be
##
## * `con`: A `SQLiteConnection` object
## * `tbl_data`: Name of the table to store the data (hash/object pairs) in.
## * `tbl_keys`: Name of the table to store the keys
##   (key/namespace/hash triplets) in.

## Taking a `SQLiteConnection` object, rather than a path, saves
## duplicating the SQLite constructor function.

## In addition, some care is required with the `hash_algorithm`
## argument -- see below.

## The SQL queries are a bit ugly but hopefully straightforward enough
## to follow.

driver_sqlite <- function(path, tbl_data = "storr_data",
                          tbl_keys = "storr_keys") {
  R6_driver_sqlite$new(path, tbl_data, tbl_keys)
}

## The R6 class definition that implements the functions above, with a
## little commentry throughout.
R6_driver_sqlite <- R6::R6Class(
  "driver_sqlite",
  public = list(
    ## Public data members
    con = NULL,
    tbl_data = NULL,
    tbl_keys = NULL,

    ## There is support for selecting different hash algorithms,
    ## However, this requires that the driver stores the used alorithm
    ## and errors if the algorithm changes, which is fiddly to set up.
    ## So this tells storr that this driver does not support setting
    ## the hash algorithm.
    traits = list(hash_algorithm = FALSE),

    ## On initialisation we'll create the two tables but only if they
    ## do not exist.  We can enforce the constraint that hash must be
    ## unique within tbl_data and key/namespace pairs must be unique
    ## within tbl_keys.
    initialize = function(con, tbl_data, tbl_keys) {
      self$con <- con
      self$tbl_data <- tbl_data
      self$tbl_keys <- tbl_keys

      sql <- c(sprintf("CREATE TABLE if NOT EXISTS %s", tbl_data),
               "(hash string PRIMARY KEY NOT NULL,",
               "value blob NOT NULL)")
      DBI::dbExecute(self$con, paste(sql, collapse = " "))

      sql <- c(sprintf("CREATE TABLE IF NOT EXISTS %s", tbl_keys),
               "(namespace string NOT NULL,",
               "key string NOT NULL,",
               "hash string NOT NULL,",
               "PRIMARY KEY (namespace, key))")
      DBI::dbExecute(self$con, paste(sql, collapse = " "))
    },

    ## This is purely for identification later.
    type = function() {
      "DBI/sqlite"
    },

    ## Total destruction of the driver; delete all data stored in both
    ## tables, then delete our database connection to render the
    ## driver useless.
    destroy = function() {
      DBI::dbRemoveTable(self$con, self$tbl_data)
      DBI::dbRemoveTable(self$con, self$tbl_keys)
      self$con <- NULL
    },

    ## Return the hash value given a key/namespace pair
    get_hash = function(key, namespace) {
      sql <- sprintf(
        'SELECT hash FROM "%s" WHERE namespace = "%s" AND key = "%s"',
        self$tbl_keys, namespace, key)
      DBI::dbGetQuery(self$con, sql)[[1]]
    },

    ## Set the key/namespace pair to a hash
    set_hash = function(key, namespace, hash) {
      sql <- c(sprintf("INSERT OR REPLACE INTO %s", self$tbl_keys),
               sprintf('(namespace, key, hash) VALUES ("%s", "%s", "%s")',
                       namespace, key, hash))
      DBI::dbExecute(self$con, paste(sql, collapse = " "))
    },

    ## Return a (deserialized) R object, given a hash
    get_object = function(hash) {
      sql <- c(sprintf("SELECT value FROM %s", self$tbl_data),
               sprintf('WHERE hash = "%s"', hash))
      value <- DBI::dbGetQuery(self$con, paste(sql, collapse = " "))[[1]]
      unserialize(value[[1]])
    },

    ## Set a (serialized) R object against a hash.  This would be
    ## considerably simpler (but probably slower and less accurate) if we
    ## serialized to string with:
    ##   rawToChar(serialize(value, NULL, TRUE))
    set_object = function(hash, value) {
      sql <- paste(sprintf("INSERT OR REPLACE INTO %s", self$tbl_data),
                   "(hash, value) VALUES (:hash, :value)")
      dat <- list(hash = hash, value = list(serialize(value, NULL)))
      DBI::dbExecute(self$con, sql, dat)
    },

    ## Check if a key/namespace pair exists.  This is somewhat more
    ## complicated than the other methods because storr assumes that
    ## this can be done for vector arguments key and namespace.  storr
    ## provides a helper function 'join_key_namespace' for predictably
    ## recycling keys and namespaces and then we consider
    ## possibilities of 0, 1 or more items to lookup.  The "more" case
    ## can be done more efficiently with a single SQL statement, but
    ## instead here we recurse.
    exists_hash = function(key, namespace) {
      nk <- storr::join_key_namespace(key, namespace)
      if (nk$n == 0L) {
        logical(0)
      } else if (nk$n == 1L) {
        sql <- sprintf('SELECT 1 FROM %s WHERE namespace = "%s" AND key = "%s"',
                       self$tbl_keys, namespace, key)
        nrow(DBI::dbGetQuery(self$con, sql)) > 0L
      } else {
        vapply(seq_len(nk$n), function(i)
          self$exists_hash(nk$key[[i]], nk$namespace[[i]]),
          logical(1), USE.NAMES = FALSE)
      }
    },

    ## Check if a hash exists.  As for 'exists_hash' this must deal
    ## with vectorised input but it's a little simpler.
    exists_object = function(hash) {
      if (length(hash) == 0L) {
        logical(0)
      } else if (length(hash) == 1L) {
        sql <- sprintf('SELECT 1 FROM %s WHERE hash = "%s"',
                       self$tbl_data, hash)
        nrow(DBI::dbGetQuery(self$con, sql)) > 0L
      } else {
        vapply(hash, self$exists_object, logical(1), USE.NAMES = FALSE)
      }
    },

    ## Delete a key.  Because of the requirement to return TRUE/FALSE on
    ## successful/unsuccessful key deletion this includes an exists_hash()
    ## step first.  As with 'exists_hash' this needs to be vectorised.
    del_hash = function(key, namespace) {
      nk <- storr::join_key_namespace(key, namespace)
      if (nk$n == 0L) {
        logical(0)
      } else if (nk$n == 1L) {
        if (self$exists_hash(key, namespace)) {
          sql <- sprintf('DELETE FROM %s WHERE namespace = "%s" AND key = "%s"',
                         self$tbl_keys, namespace, key)
          DBI::dbExecute(self$con, sql)
          TRUE
        } else {
          FALSE
        }
      } else {
        vapply(seq_len(nk$n), function(i)
          self$del_hash(nk$key[[i]], nk$namespace[[i]]),
          logical(1), USE.NAMES = FALSE)
      }
    },

    ## Delete a hash
    del_object = function(hash) {
      if (length(hash) == 0L) {
        logical(0)
      } else if (length(hash) == 1L) {
        if (self$exists_object(hash)) {
          sql <- sprintf(
            'DELETE FROM %s WHERE hash = "%s"', self$tbl_data, hash)
          DBI::dbExecute(self$con, sql)
          TRUE
        } else {
          FALSE
        }
      } else {
        vapply(hash, self$del_object, logical(1), USE.NAMES = FALSE)
      }
    },

    ## List hashes, namespaces and keys.  Because the SQLite driver seems to
    ## return numeric(0) if the result set is empty, we need as.character here.
    list_hashes = function() {
      sql <- sprintf("SELECT hash FROM %s", self$tbl_data)
      as.character(DBI::dbGetQuery(self$con, sql)[[1]])
    },

    list_namespaces = function() {
      sql <- sprintf("SELECT DISTINCT namespace FROM %s", self$tbl_keys)
      as.character(DBI::dbGetQuery(self$con, sql)[[1]])
    },

    list_keys = function(namespace) {
      sql <- sprintf('SELECT key FROM %s WHERE namespace = "%s"',
                     self$tbl_keys, namespace)
      as.character(DBI::dbGetQuery(self$con, sql)[[1]])
    }
  ))

## Next, let's give the driver a little workout.
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
dr <- driver_sqlite(con)

## Start with the hash part of the database.  At first we have no
## hashes in the database:
dr$list_hashes()

## so `exists_object` returns `FALSE`:
hash <- digest::digest(mtcars)
dr$exists_object(hash)

## We can set an object against a hash:
dr$set_object(hash, mtcars)

## and then `exists_object` will return `TRUE`
dr$exists_object(hash)

## and we can retrieve the object:
head(dr$get_object(hash))

## Our set of hashes:
dr$list_hashes()

## Delete the hash:
dr$del_object(hash)

## And it's gone:
dr$list_hashes()
dr$exists_object(hash)

## Set up a key
key <- "aaa"
namespace <- "ns"
dr$set_hash(key, namespace, hash)

## Which now exists:
dr$exists_hash(key, namespace)

## And can be listed:
dr$list_keys(namespace)

## and the hash against the key returned:
dr$get_hash(key, namespace)

dr$del_hash(key, namespace)
dr$exists_hash(key, namespace)
dr$list_keys(namespace)

## OK, so this *seems* to be working.  But how do we test if it is
## actually working?  `storr` provides an automatic testing facility
## based on `testthat`.

## To do this, we provide a function that takes one argument `dr`; if
## this is `NULL` then we must create a new empty driver, if
## non-`NULL` we must create a driver pointing at the same storage as
## an existing driver `dr`:
create_sqlite <- function(dr = NULL) {
  if (is.null(dr)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    con <- dr$con
  }
  driver_sqlite(con)
}

## (because this function will repeatedly create new databases,
## `:memory:` is a good path here)!

## Pass this in to `storr::test_driver`
storr::test_driver(create_sqlite)

## Now that the driver works, we can write the wrapper function:
storr_sqlite <- function(con,
                         tbl_data = "storr_data", tbl_keys = "storr_keys",
                         default_namespace = "objects") {
  storr::storr(driver_sqlite(con, tbl_data, tbl_keys),
               default_namespace)
}

## and construct a `storr` with it:
st_sql <- storr_sqlite(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))

## Nothing in the storr:
st_sql$list()

## Set some data:
st_sql$set("foo", runif(10))

## Retrieve it:
st_sql$get("foo")

## Delete it:
st_sql$del("foo")
st_sql$list()

## Underlying data is still kicking around:
st_sql$list_hashes()
st_sql$get_value(st_sql$list_hashes())

## But we can garbage collect:
st_sql$gc()
st_sql$list_hashes()

st_sql$destroy()

## This is not really SQL's strong suit.  But if key/value storage is
## a small part of an application that already uses SQLite for storage
## then this approach could be a sensible move.
