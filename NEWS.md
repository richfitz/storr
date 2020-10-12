## storr 1.2.4 (2020-10-12)

* Speed up the `$get_hash()` method of RDS drivers using C code and traits (#96, #98, @wlandau).

## storr 1.2.1 (2018-10-18)

* Avoid a race condition in writing to rds storrs in parallel (#80, reported by @wlandau)

## storr 1.2.0 (2018-05-31)

* New index function (#22, requested by @wlandau)
* Support for creating storrs with different storage for keys and data (`driver_multistorr` and `storr_multistorr`, #67)
* New repair methods (`$check` and `$repair`) for detecting some problems with storr repositories.  This is a work in progress and is currently only supported for the rds driver.
* Support infrastructure for "remote storr" databases, which can be used to create a storr of rds objects on a remote location (#61).  This does not implement any useful remote storrs, but only provides support for doing so.

## storr 1.1.3 (2017-12-15)

* Only use version 2 serialisation, avoiding breakage when version 3 is released in 3.5.0 (#62, reported by Tomas Kalibera)
* Quote sql more safely (#60, reported by @wlandau)
* Support for duplicating (`$duplicate`) and filling (`$fill`) keys with identical values, without duplicating or reserialising data (#56, requested by @wlandau)
* Moved to a shiny new [pkgdown](https://pkgdown.r-lib.org/) website - https://richfitz.github.com/storr

## storr 1.1.2 (2017-09-08)

* Use the full path so `storr`s still work when the the working directory changes (#50, #51, @wlandau)
* Avoid DBI disconnection warnings when running tests
* Avoid running Redis tests in inappropriate settings (#52, #53, thanks @wlandau)
* Move Redis code entirely into redux

## storr 1.1.1 (2017-06-02)

* Fix bug padding of base64 encoded filenames (exposed only when using vectorised interface

## storr 1.1.0 (2017-05-05)

* Support for using relational databases via DBI (SQLite, Postgres) #23 - MySQL and Microsoft SQL Server support is not implemented yet
* Support for selecting hash algorithm at the driver level, so rather than being limited to md5, more robust algorithms can be used.  This is implemented for all storr drivers, and for the rds driver will safely work on storr databases from before this version
* `mget`/`mset`/`mset_by_value` added for vectorised `get`/`set`/`set_by_value` operations that reduce the number of roundtrips on some drivers.  `del` and `exists` become vectorised at the same time. This requires extensive change to drivers but is backward compatible for users.
* Large object support for rds is slightly improved (#25) but still very slow
* Storr now supports exporting multiple namespaces at once (possibly all namespaces) making it easier migrate large amounts of data between storrs.  This does cause a small breaking change where `$import()` now returns a matrix with columns `name`, `namespace` indicating what was imported (previously this was just a vector)
* Storr import/export more gracefully handles missing keys
* RDS storrs no longer pad base64 filenames with trailing with `=` which reduces the number of possily problematic characters.  Existing storrs are unaffected (#43)
* RDS storrs are not left in an inconsistent state if RDS file writing fails (#40, thanks @krlmlr)

## storr 1.0.1 (2016-05-10)

* Updated to work with recent testthat (>= 1.0.0)
* Limited support for writing out very large serialised objects (rds)

## storr 1.0.0 (2016-01-19)

* Initial CRAN release
