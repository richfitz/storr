# storr

[![Build Status](https://travis-ci.org/richfitz/storr.png?branch=master)](https://travis-ci.org/richfitz/storr)
[![](http://www.r-pkg.org/badges/version/storr)](http://cran.r-project.org/package=storr)

Simple object cacher for R.  `storr` acts as a very simple key-value store (supporting `get`/`set`/`del` for arbitrary R objects as data and as keys).  The actual storage can be transient or persistent, local or distributed without changing the interface.  To allow for distributed access, data is returned by *content* rather than simply by key (with a key/content lookup step) so that if another process changes the data, `storr` will retrieve the current version.

* Cached in-memory copies that might be faster to retrieve than on-disk/database copies
* Content-addressable storage, storing and retrieving potentially fewer copies of identical data (useful if lookup is slow or over a network) and to make the system somewhat robust in the face of multiple accessing processes
* Fetch from an external source (e.g. website) if a key is not found locally
* Pluggable storage backends - currently
  - environment (memory)
  - rds (disk)
  - Redis (`http://redis.io`) (via [redux](https://github.com/richfitz/redux) & [RedisAPI](https://github.com/ropensci/RedisAPI))
  - [rlite](https://github.com/seppo0010/rlite) (via [rrlite](https://github.com/ropensci/rrlite) & [RedisAPI](https://github.com/ropensci/RedisAPI))
* Future backends might include
  - git via [git2r](https://github.com/ropensci/git2r)
  - leveldb via [RcppLevelDB](https://github.com/gokceneraslan/rcppleveldb)
  - [dat](http://dat-data.com)

`storr` always goes back to the common storage (database, filesystem, whatever) for the current object -> hash mapping but when retrieving the data given a hash hash we can often do that without accessing the underlying storage.  This means that repeated lookups happen quickly while still being able to reflect changes elsewhere.

# Installation

From CRAN

```r
install.packages("storr")
```

or install the development version with

```
devtools::install_github("richfitz/storr")
```

# Documentation

`storr` comes with three vignettes:

* [storr](http://richfitz.github.io/storr/vignettes/storr.html) `vignette("storr")` outlines basic use and core implementation details.
* [external](http://richfitz.github.io/storr/vignettes/external.html) `vignette("external")` shows how to use storr to cache external resources such as files, web resources, etc, using the `storr_external` object.
* [drivers](http://richfitz.github.io/storr/vignettes/drivers.html) `vignette("drivers")` shows how to create new drivers for storr, illustrated by implementing a naive driver for DBI/SQLite.
