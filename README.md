# storr

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![Build Status](https://travis-ci.org/richfitz/storr.svg?branch=master)](https://travis-ci.org/richfitz/storr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/richfitz/storr?branch=master&svg=true)](https://ci.appveyor.com/project/richfitz/storr)
[![codecov.io](https://codecov.io/github/richfitz/storr/coverage.svg?branch=master)](https://codecov.io/github/richfitz/storr?branch=master)
[![](http://www.r-pkg.org/badges/version/storr)](https://cran.r-project.org/package=storr)

Simple object cacher for R.  `storr` acts as a very simple key-value store (supporting `get`/`set`/`del` for arbitrary R objects as data).  The actual storage can be transient or persistent, local or distributed without changing the interface.  To allow for distributed access, data is returned by *content* rather than simply by key (with a key/content lookup step) so that if another process changes the data, `storr` will retrieve the current version.

* Cached in-memory copies that might be faster to retrieve than on-disk/database copies
* Content-addressable storage, storing and retrieving potentially fewer copies of identical data (useful if lookup is slow or over a network) and to make the system somewhat robust in the face of multiple accessing processes
* Fetch from an external source (e.g. website) if a key is not found locally
* Pluggable storage backends - currently
  - environment (memory)
  - rds (disk)
  - [DBI](https://cran.r-project.org/package=DBI) though which you can use:
    * [SQLite](https://sqlite.org/index.html) (via [RSQLite](https://cran.r-project.org/package=RSQLite))
    * [Postgres](https://www.postgresql.org/) (via [RPostgres](https://github.com/r-dbi/RPostgres))
  - Redis (provided by [redux](https://github.com/richfitz/redux))
  - [rlite](https://github.com/seppo0010/rlite) (via [rrlite](https://github.com/ropensci/rrlite), interface function in [redux](https://github.com/richfitz/redux))
  - An on disk memory-mapped file interface is implemented in [thor](https://github.com/richfitz/thor)

`storr` always goes back to the common storage (database, filesystem, whatever) for the current object to hash mapping, ensuring consistency when using multiple processes.  However, when retrieving or writing the data given a hash we can often avoid accessing the underlying storage.  This means that repeated lookups happen quickly while still being able to reflect changes elsewhere; time savings can be substantial where large objects are being stored.

# Installation

From CRAN

```r
install.packages("storr")
```

or install the development version with

```
remotes::install_github("richfitz/storr@develop", upgrade = FALSE)
```

# Documentation

`storr` comes with two vignettes:

* [storr](https://richfitz.github.io/storr/articles/storr.html) `vignette("storr")` outlines basic use and core implementation details.
* [external](https://richfitz.github.io/storr/articles/external.html) `vignette("external")` shows how to use storr to cache external resources such as files, web resources, etc, using the `storr_external` object.
