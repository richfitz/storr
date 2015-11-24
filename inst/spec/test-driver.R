## This requires
##   .driver_name: character(1)
##   .driver_create: function()
context(sprintf("drivers [%s]", .driver_name))

test_that("basic (empty)", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  ns <- "objects"
  expect_that(dr$list_hashes(), equals(character(0)))
  expect_that(dr$list_keys(ns), equals(character(0)))

  expect_that(dr$get_value("aaa"),
              throws_error("hash 'aaa' not found"))
  expect_that(dr$get_hash("aaa", ns),
              throws_error("key 'aaa' not found"))

  expect_that(dr$del_hash("aaa"), is_false())
  expect_that(dr$del_key("aaa", ns),  is_false())

  expect_that(dr$exists_hash("aaa"), is_false())
  expect_that(dr$exists_key("aaa", ns), is_false())
})

test_that("set", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  ns <- "objects"

  ## First, let's set some data to a hash
  d <- runif(100)

  dr$set_hash_value("aaa", d)
  expect_that(dr$get_value("aaa"), equals(d, tolerance=1e-15))

  ## Then, set a key to address that hash:
  dr$set_key_hash("bbb", "aaa", ns)
  expect_that(dr$get_hash("bbb", ns), is_identical_to("aaa"))

  expect_that(dr$exists_hash("aaa"), is_true())
  expect_that(dr$exists_key("bbb", ns), is_true())

  ## Then delete the key:
  expect_that(dr$del_key("bbb", ns), is_true())
  expect_that(dr$get_hash("bbb", ns), throws_error("key 'bbb' not found"))

  ## And delete the hash:
  expect_that(dr$del_hash("aaa"), is_true())
  expect_that(dr$get_value("aaa"), throws_error("hash 'aaa' not found"))
})

test_that("namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  ns <- "objects"

  ## First, let's set some data to a hash
  d <- runif(100)

  dr$set_hash_value("aaa", d)
  expect_that(dr$get_value("aaa"), equals(d, tolerance=1e-15))

  dr$set_key_hash("bbb", "aaa", ns)
  expect_that(dr$get_hash("bbb", ns),
              is_identical_to("aaa"))

  expect_that(dr$list_keys(ns), equals("bbb"))
  expect_that(dr$exists_key("bbb", ns), is_true())

  expect_that(dr$del_key("bbb", ns), is_true())
  expect_that(dr$exists_key("bbb", ns), is_false())

  ## Save into a different namespace:
  ns2 <- "another"
  dr$set_key_hash("ccc", "aaa", ns2)
  expect_that(dr$get_hash("ccc", ns2), is_identical_to("aaa"))

  expect_that(dr$list_keys(ns), equals(character(0)))
  expect_that(dr$list_keys(ns2), equals("ccc"))
  expect_that(dr$exists_key("ccc", ns), is_false())
  expect_that(dr$exists_key("ccc", ns2), is_true())

  expect_that(dr$del_key("ccc", ns2), is_true())
  expect_that(dr$exists_key("ccc", ns2), is_false())
})
