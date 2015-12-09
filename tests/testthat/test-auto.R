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
