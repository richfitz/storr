context("environment driver")

## There was a bug in reusing an environment driver caused the
## destruction of all data held in an environment storr.
test_that("shared environment regression check", {
  st1 <- storr_environment()
  st1$set("a", 1)

  st2 <- storr(driver_environment(st1$driver$envir))
  expect_identical(st1$list(), "a")
  expect_identical(st2$list(), "a")
  st2$set("b", 2)

  st3 <- storr_environment(st1$driver$envir)
  cmp <- sort(c("a", "b"))
  expect_identical(sort(st1$list()), cmp)
  expect_identical(sort(st2$list()), cmp)
  expect_identical(sort(st3$list()), cmp)
})
