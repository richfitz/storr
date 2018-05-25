context("driver multistorr details")


test_that("creation", {
  path <- tempfile()
  st <- storr_multistorr(driver_environment(), driver_rds(path))
  expect_equal(st$driver$type(), "multistorr (keys: environment, data: rds)")
  st$set("a", mtcars)

  cmp <- storr_rds(path)
  expect_equal(cmp$list_hashes(), st$list_hashes())
  expect_equal(cmp$list(), character())
  expect_equal(st$list(), "a")

  st$destroy()
  expect_equal(cmp$list_hashes(), character())
})


test_that("storr driver detection", {
  e <- driver_environment()
  expect_silent(assert_probably_storr_driver(e))
  expect_error(assert_probably_storr_driver(e$type),
               "does not look like a storr driver")
})
