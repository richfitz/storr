# cacher

[![Build Status](https://travis-ci.org/richfitz/cacher.png?branch=master)](https://travis-ci.org/richfitz/cacher)

Object caching.  Based on ideas in both [remake](https://github.com/richfitz/remake) and [rrqueue](https://github.com/traitecoevo/rrqueue).  Featuring

* cached in-memory copies that might be faster to retrieve than on-disk copies
* content-addressable storage, storing potentially fewer copies of identical data
* Pluggable backends - currently
  - environment (memory)
  - rds (disk)
  - [Redis](http://redis.io) (via [RcppRedis](https://http://cran.r-project.org/web/packages/RcppRedis/index.html)/[RedisAPI](https://github.com/ropensci/RedisAPI))
  - [rlite](https://github.com/seppo0010/rlite) (via [rrlite](https://github.com/ropensci/rrlite))
* Future backends might include
  - git via [git2r](https://github.com/ropensci/git2r)
