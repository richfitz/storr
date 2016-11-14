## storr 1.1.0 (2016-??-??)

* Support for using relational databases via DBI (SQLite, MySQL, Postgres) #23
* Support for selecting hash algorithm at the driver level, so rather than being limited to md5, more robust algorithms can be used.  This is implemented for all storr drivers, and for the rds driver will safely work on storr databases from before this version

## storr 1.0.1 (2016-05-10)

* Updated to work with recent testthat (>= 1.0.0)
* Limited support for writing out very large serialised objects (rds)

## storr 1.0.0 (2016-01-19)

* Initial CRAN release
