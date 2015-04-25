# storr

[![Build Status](https://travis-ci.org/richfitz/storr.png?branch=master)](https://travis-ci.org/richfitz/storr)

Object caching.  Based on ideas in both [remake](https://github.com/richfitz/remake) and [rrqueue](https://github.com/traitecoevo/rrqueue).  Featuring

* cached in-memory copies that might be faster to retrieve than on-disk copies
* content-addressable storage, storing and retrieving potentially fewer copies of identical data
* Pluggable backends - currently
  - environment (memory)
  - rds (disk)
  - [Redis](http://redis.io) (via [RcppRedis](https://http://cran.r-project.org/web/packages/RcppRedis/index.html)/[RedisAPI](https://github.com/ropensci/RedisAPI))
  - [rlite](https://github.com/seppo0010/rlite) (via [rrlite](https://github.com/ropensci/rrlite))
* Future backends might include
  - git via [git2r](https://github.com/ropensci/git2r)

We always go back to the common storage (database, filesystem, whatever) for the current object -> hash mapping but when retrieving a hash we can often do that without hitting the underlying storage.  This means that repeated lookups happen very quickly while still being able to reflect change elsewhere.
