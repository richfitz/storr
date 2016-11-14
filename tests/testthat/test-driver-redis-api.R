if ("redux" %in% .packages(TRUE)) {
  context("redis")
  test_that("storr", {
    rand_str <- function() paste0(hash_object(Sys.time()), ":")
    con <- redux::hiredis()
    st <- storr_redis_api(rand_str(), con)
    expect_is(st, "storr")
    expect_equal(st$driver$type(), "redis_api/redux")
  })
}
