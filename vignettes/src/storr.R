## ---
## title: "storr"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{storr}
##   %\VignetteEngine{knitr::rmarkdown}
##   \usepackage[utf8]{inputenc}
## ---

library(storr)

## `storr` provides very simple key/value stores for R.  They attempt
## to provide the most basic set of key/value lookup functionality
## that is completely consistent across a range of different
## underlying storage drivers (in memory storage, filesystem and
## proper database).  All the storage is _content addressable_, so
## keys map onto hashes and hashes map onto data.

## The `rds` driver stores contents at some path by saving out to rds
## files.  Here I'm using a temporary directory for the path; the
## driver will create a number of subdirectories here.
path <- tempfile("storr_")
st <- storr::storr_rds(path)

## Alternatively you can create the driver explicitly:
###+ eval=FALSE
dr <- storr::driver_rds(path)
st <- storr::storr(dr)

## With this driver object we can create the `storr` object which is
## what we actually interact with:

## # Key-value store:

## The most simple way of interacting with a `storr` object is
## `get`/`set`/`del` for getting, setting and deleting data stored at
## some key.  To store data:
st$set("mykey", mtcars)

## To get the data back
head(st$get("mykey"))

## What is in the `storr`?
st$list()

## Or, much faster, test for existance of a particular key:
st$exists("mykey")

st$exists("another_key")

## To delete it:
st$del("mykey")

## It's gone!
st$list()

## though the actual data is still stored in the database:
st$list_hashes()
head(st$get_value(digest::digest(mtcars)))

## though now that there are no keys pointing at the data is is
## subject to garbage collection:
del <- st$gc()
del

st$list_hashes()

## # Import / export

## Objects can be imported in and exported out of a `storr`;

## Import from a list, environment or another `storr`
st$import(list(a=1, b=2))
st$list()
st$get("a")

## Export to an environment or another `storr`
e <- new.env(parent=emptyenv())
st$export(e)
ls(e)
e$a

## Convenience function that does the same as the above (exports to a
## new environment that has `.GlobalEnv` as its parent)
e2 <- st$to_environment()
ls(e2)
parent.env(e2)

st2 <- storr::storr(driver=storr::driver_rds(tempfile("storr_")))
st2$list()
st2$import(st)
st2$list()

## # Supported backends

## * environments (`driver_environment`) - mostly for debugging and
## transient storage, but by far the fastest.
## * on disk with rds (`driver_rds`) - zero dependencies, quite fast,
## will suffer under high concurrency because there is no file
## locking.
## * Redis (`driver_redis`) - uses
## [`redux`](https://github.com/richfitz/redux) to store
## the data in a Redis (`http://redis.io`) database.  Slower than rds,
## but can allow multiple R processes to share the same set of objects.
## * rlite (`driver_rlite`) - stores data in an
## [rlite](https://github.com/seppo0010/rlite) using
## [`rrlite`](https://github.com/ropensci/rrlite).  This is the
## slowest at present and does not support concurrency at all.  But
## rlite has the potential to be as useful as SQLite is so this will
## improve.

## # Implementation details

## `storr` includes a few useful features that are common to all
## drivers.

## ## Content addressable lookup

## The only thing that is stored against a key is the hash of some
## object.  Each driver does this a different way, but for the rds
## driver it stores small text files that list the hash in them.  So:
dir(file.path(path, "keys", "objects"))
readLines(file.path(path, "keys", "objects", "a"))
st$get_hash("a")

## Then there is one big pool of hash / value pairs:
st$list_hashes()

## in the rds driver these are stored like so:
dir(file.path(path, "data"))

## ## Environment-based caching

## Every time data passes across a `get` or `set` method, `storr`
## stores the data in an environment within the `storr` object.
## Because we store the content against its hash, it's always in sync
## with what is saved to disk.  That means that the look up process
## goes like this:
##
## 1. Ask for a key, get returned the hash of the content
## 2. Check in the caching environment for that hash and return that
## if present
## 3. If not present, read content from disk/db/wherever the driver
## stores it and save it into the caching environment
##
## Because looking up data in the environment is likely to be orders
## of magnitide faster than reading from disks or databases, this
## means that commonly accessed data will be accessed at a similar
## speed to native R objects, while still immediately reflecting
## changes to the content (because that would mean the hash changes)

## To demonstrate:
st <- storr::storr(driver=storr::driver_rds(tempfile("storr_")))

## This is the caching environent; currently empty
ls(st$envir)

## Set some key to some data:
set.seed(2)
st$set("mykey", runif(100))

## The environment now includes an object with a *name* that is the
## same as the *hash* of its contents:
ls(st$envir)

## Extract the object from the environment and hash it
storr:::hash_object(st$envir[[ls(st$envir)]])

## When we look up the value stored against key `mykey`, the first
## step is to check the key/hash map; this returns the key above (this
## step *does* involve reading from disk)
st$get_hash("mykey")

## It then calls `$get_value` to extract the value associated with
## that hash - the first thing that function does is try to locate the
## hash in the environment, otherwise it reads the data from wherever
## the driver stores it.
st$get_value

## The speed up is going to be fairly context dependent, but 10x seems
## pretty good in this case (some of the overhead is simply a longer
## code path as we call out to the driver).
hash <- st$get_hash("mykey")
microbenchmark::microbenchmark(st$get_value(hash, use_cache=TRUE),
                               st$get_value(hash, use_cache=FALSE))
