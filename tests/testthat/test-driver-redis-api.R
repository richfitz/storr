if ("redux" %in% .packages(TRUE)) {
  context("redis")
  test_that("storr", {
    st <- storr_redis_api(rand_str(), redux::hiredis())
    on.exit(st$destroy())
    expect_is(st, "storr")
    expect_equal(st$driver$type(), "redis_api/redux")
  })
}
