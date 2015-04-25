context("driver (Redis)")

## List functions are:
##   is_list
##   length_list
##   set_key_hash_list
##   get_hash_list
##   del_hash_list

## Redis specific things.  Eventually will be list specific and then
## I'll implement for all drivers.
test_that("Lists on none", {
  dr <- driver_redis("cacher")
  dr$flush()

  key <- "aaa"
  value <- "xxx"

  expect_that(dr$exists_key(key), is_false())

  expect_that(dr$is_list(key), is_false())
  expect_that(dr$length_list(key), equals(0))
  expect_that(dr$get_hash_list(key), equals(character(0)))

  expect_that(dr$get_hash_list(key, 1),
              throws_error("1 is out of bounds"))
  expect_that(dr$get_hash_list(key, -1),
              throws_error("-1 is out of bounds"))
  expect_that(dr$get_hash_list(key, c(1, 2)),
              throws_error("1, 2 is out of bounds"))

  expect_that(dr$set_key_hash_list(key, value, 1),
              throws_error("Wrong type: expected list, recieved none"))
  expect_that(dr$set_key_hash_list(key, value, -1),
              throws_error("Wrong type: expected list, recieved none"))

  expect_that(dr$del_hash_list(key), not(throws_error()))
})

test_that("Lists on objects", {
  dr <- driver_redis("cacher")
  dr$flush()

  key <- "aaa"
  value <- "xxx"

  object_cache(dr)$set(key, 1:10)

  expect_that(dr$exists_key(key), is_true())

  expect_that(dr$is_list(key), is_false())
  expect_that(dr$length_list(key), equals(0))
  expect_that(dr$get_hash_list(key), equals(character(0)))

  expect_that(dr$get_hash_list(key, 1),
              throws_error("1 is out of bounds"))
  expect_that(dr$get_hash_list(key, -1),
              throws_error("-1 is out of bounds"))
  expect_that(dr$get_hash_list(key, c(1, 2)),
              throws_error("1, 2 is out of bounds"))

  expect_that(dr$set_key_hash_list(key, value, 1),
              throws_error("Wrong type: expected list, recieved none"))
  expect_that(dr$set_key_hash_list(key, value, -1),
              throws_error("Wrong type: expected list, recieved none"))

  expect_that(dr$del_hash_list(key), not(throws_error()))
})

test_that("Actual lists!", {
  dr <- driver_redis("cacher")
  dr$flush()

  key <- "aaa"
  value <- 1:5
  hash <- vcapply(value, hash_object)

  dr$set_key_hash_list(key, hash, NULL)

  expect_that(dr$is_list(key), is_true())
  expect_that(dr$length_list(key), equals(length(value)))
  expect_that(dr$get_hash_list(key), equals(hash))

  expect_that(dr$get_hash_list(key, 1),
              equals(hash[[1]]))
  expect_that(dr$get_hash_list(key, -1),
              throws_error("-1 is out of bounds"))
  expect_that(dr$get_hash_list(key, c(1, 2)),
              equals(hash[c(1, 2)]))

  hash2 <- hash_object(6)
  expect_that(dr$set_key_hash_list(key, hash2, 1),
              not(throws_error()))
  expect_that(dr$get_hash_list(key, 1),
              equals(hash2))

  expect_that(dr$set_key_hash_list(key, value, -1),
              throws_error("-1 is out of bounds"))

  expect_that(dr$del_hash_list(key), not(throws_error()))
})
