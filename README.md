# storr

[![Build Status](https://travis-ci.org/richfitz/storr.png?branch=master)](https://travis-ci.org/richfitz/storr)

Simple object cacher for R.  `storr` acts as a very simple key-value store (supporting `get`/`set`/`del` for arbitrary R objects as data and as keys).  The actual storage can be transient or persistent, local or distributed without changing the interface.  To allow for distributed access, data is returned by *content* rather than simply by key (with a key/content lookup step) so that if another process changes the data, `storr` will retrieve the current version.

* Cached in-memory copies that might be faster to retrieve than on-disk copies
* Content-addressable storage, storing and retrieving potentially fewer copies of identical data (useful if lookup is slow or over a network)
* Indexable serialisation of list-like objects allow random access reads and writes (suitable for breaking objects up for use across a distributed computing environment).
* Fetch from an external source (e.g. website) if a key is not found locally
* Pluggable backends - currently
  - environment (memory)
  - rds (disk)
  - [Redis](http://redis.io) (via [redux](https://github.com/richfitz/redux) & [RedisAPI](https://github.com/ropensci/RedisAPI))
  - [rlite](https://github.com/seppo0010/rlite) (via [rrlite](https://github.com/ropensci/rrlite) & [RedisAPI](https://github.com/ropensci/RedisAPI))
* Future backends might include
  - git via [git2r](https://github.com/ropensci/git2r)
  - leveldb via [RcppLevelDB](https://github.com/gokceneraslan/rcppleveldb)
  - [dat](http://dat-data.com)

We always go back to the common storage (database, filesystem, whatever) for the current object -> hash mapping but when retrieving a hash we can often do that without hitting the underlying storage.  This means that repeated lookups happen very quickly while still being able to reflect change elsewhere.

# Installation

```
devtools::install_github("richfitz/storr")
```

# Documentation

There is a vignette (`vignette("storr")`) that outlines the basic idea; see [the website](http://richfitz.github.io/storr/vignettes/storr.html).
