## .driver_create <- driver_environment

## .driver_create <- function() driver_rds(tempfile("storr_"))

if (FALSE) {
  rand_str <- function() paste0(hash_object(Sys.time()), ":")
  con <- redux::hiredis()
  .driver_create <-  function() driver_redis_api(rand_str(), con)
}
storr:::test_driver("environment", function() driver_environment())
storr:::test_driver("rds", function() driver_rds(tempfile("storr_")))

## These are not required on CRAN testing, but only for my own
## edification.
if ("redux" %in% .packages(TRUE)) {
  rand_str <- function() paste0(hash_object(Sys.time()), ":")
  con <- redux::hiredis()
  storr:::test_driver("redis_api/redux",
                      function() driver_redis_api(rand_str(), con))
}
if ("rrlite" %in% .packages(TRUE)) {
  rand_str <- function() hash_object(Sys.time())
  con <- rrlite::hirlite()
  storr:::test_driver("redis_api/rrlite",
                      function() driver_redis_api(rand_str(), con))
}

## ## Minimal example of locking:
## .test_prep()
## st <- storr(driver_environment(), default_namespace="foo")
## .test_prep()
## st <- storr(driver_environment(), mangle_key=TRUE)
## .driver_create <- driver_environment

## TODO: list namespaces, otherwise how do we get the set of
## namespaces to do gc?  That does require changes to the gc code too.
