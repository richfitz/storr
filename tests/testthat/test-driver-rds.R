context("driver_rds")

test_that("creation", {
  path <- tempfile()
  expect_that(file.exists(path), is_false())
  dr <- driver_rds(path)
  on.exit(unlink(path, recursive=TRUE))

  expect_that(file.exists(path), is_true())
  expect_that(dir(path), equals(c("data", "keys")))
  expect_that(dir(file.path(path, "data")), equals(character(0)))
  expect_that(dir(file.path(path, "keys")), equals(character(0)))
})

## These will be done for all sorts of things...
test_that("basic empty", {
  path <- tempfile()
  dr <- driver_rds(path)
  on.exit(unlink(path, recursive=TRUE))

  expect_that(dr$list_hashes(), equals(character(0)))
  expect_that(dr$list_keys(),   equals(character(0)))

  expect_that(dr$get_data("aaa"),
              throws_error("hash 'aaa' not found"))
  expect_that(dr$get_hash("aaa"),
              throws_error("key 'aaa' not found"))

  expect_that(dr$del_hash("aaa"), is_false())
  expect_that(dr$del_key("aaa"),  is_false())

  expect_that(dr$exists_hash("aaa"), is_false())
  expect_that(dr$exists_key("aaa"), is_false())
})

test_that("set", {
  path <- tempfile()
  dr <- driver_rds(path)
  on.exit(unlink(path, recursive=TRUE))

  ## First, let's set some data to a hash
  d <- runif(100)
  dr$set_hash_value("aaa", d)
  expect_that(dr$get_data("aaa"), is_identical_to(d))

  ## Then, set a key to address that hash:
  dr$set_key_hash("bbb", "aaa")
  expect_that(dr$get_hash("bbb"), is_identical_to("aaa"))

  expect_that(dr$exists_hash("aaa"), is_true())
  expect_that(dr$exists_key("bbb"), is_true())

  ## Then delete the key:
  expect_that(dr$del_key("bbb"), is_true())
  expect_that(dr$get_hash("bbb"), throws_error("key 'bbb' not found"))

  ## And delete the hash:
  expect_that(dr$del_hash("aaa"), is_true())
  expect_that(dr$get_data("aaa"), throws_error("hash 'aaa' not found"))
})
