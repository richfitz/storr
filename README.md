# storr

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/richfitz/storr.svg?branch=master)](https://travis-ci.org/richfitz/storr)
[![codecov.io](https://codecov.io/github/richfitz/storr/coverage.svg?branch=master)](https://codecov.io/github/richfitz/storr?branch=master)
[![](http://www.r-pkg.org/badges/version/storr)](https://cran.r-project.org/package=storr)

Simple object cacher for R.  `storr` acts as a very simple key-value store (supporting `get`/`set`/`del` for arbitrary R objects as data).  The actual storage can be transient or persistent, local or distributed without changing the interface.  To allow for distributed access, data is returned by *content* rather than simply by key (with a key/content lookup step) so that if another process changes the data, `storr` will retrieve the current version.

* Cached in-memory copies that might be faster to retrieve than on-disk/database copies
* Content-addressable storage, storing and retrieving potentially fewer copies of identical data (useful if lookup is slow or over a network) and to make the system somewhat robust in the face of multiple accessing processes
* Fetch from an external source (e.g. website) if a key is not found locally
* Pluggable storage backends - currently
  - environment (memory)
  - rds (disk, AWS S3)
  - [DBI](https://cran.r-project.org/package=DBI) though which you can use:
    * [SQLite](https://sqlite.org) (via [RSQLite](https://cran.r-project.org/package=RSQLite))
    * [Postgres](https://postgresql.org) (via
[RPostgresSQL](https://cran.r-project.org/package=RPostgreSQL) or [RPostgres](https://github.com/rstats-db/RPostgres))
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

## RDS on AWS S3

*This section should be moved elsewhere if the real storr package decides to merge this work*

To use an S3 bucket behind an rds store, you must have an AWS access key id and matching secret access key. These may be generated from the [AWS console](https://docs.aws.amazon.com/general/latest/gr/managing-aws-access-keys.html). The are a number of ways to make these access keys accessible, detailed in the [aws.s3 package](https://github.com/cloudyr/aws.s3) documentation. We recommend placing the credentials in `~/aws/credentials` in the following format:

```
[default]
aws_access_key_id = your_id
aws_secret_access_key = your_secret_key
```
