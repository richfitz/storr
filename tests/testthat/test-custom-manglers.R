context("test-custom-manglers")

test_that("ignore registered mangler on a new storr", {
  on.exit(options(storr_mangler = NULL))
  do.call(register_mangler, test_mangler)
  s <- storr_rds(tempfile(), mangle_key = "none")
  test_key(s, "a", "a")
})

test_that("use registered mangler on a new storr", {
  on.exit(options(storr_mangler = NULL))
  do.call(register_mangler, test_mangler)
  s <- storr_rds(tempfile(), mangle_key = "test_mangler")
  test_key(s, "a", "test_mangler_a")
})

test_that("recover an old custom mangler", {
  on.exit(options(storr_mangler = NULL))
  do.call(register_mangler, test_mangler)
  s <- storr_rds(tempfile(), mangle_key = "test_mangler")
  test_key(s, "a", "test_mangler_a")
  s <- storr_rds(s$driver$path, mangle_key = "test_mangler")
  expect_equal(s$get("a"), "x")
  test_key(s, "b", "test_mangler_b")
  s <- storr_rds(s$driver$path)
  expect_equal(s$get("a"), "x")
  expect_equal(s$get("b"), "x")
  test_key(s, "c", "test_mangler_c")
})

test_that("unregistered mangler and a new storr", {
  expect_null(getOption("storr_mangler"))
  expect_error(
    storr_rds(tempfile(), mangle_key = "test_mangler"),
    regex = "not registered"
  )
})

test_that("new mangler registry contradicts old mangler", {
  on.exit(options(storr_mangler = NULL))
  do.call(register_mangler, test_mangler)
  s <- storr_rds(tempfile(), mangle_key = "test_mangler")
  do.call(
    register_mangler,
    list(name = "test_mangler_2", encode = I, decode = I)
  )
  test_key(s, "a", "test_mangler_a")
})

test_that("new custom mangler conflicts with old preset", {
  on.exit(options(storr_mangler = NULL))
  for (mangle_key in c("none", "base64", "test_mangler")) {
    if (identical(mangle_key, "test_mangler")) {
      do.call(register_mangler, test_mangler)
    }
    s <- storr_rds(tempfile(), mangle_key = mangle_key)
    with_options(list(storr_mangler = test_mangler), {
      expect_error(
        s <- storr_rds(s$driver$path, mangle_key = "test_mangler_2"),
        regex = "Incompatible value for mangle_key"
      )
    })
  }
  expect_error(
    s <- storr_rds(s$driver$path, mangle_key = TRUE),
    regex = "Incompatible value for mangle_key"
  )
})
