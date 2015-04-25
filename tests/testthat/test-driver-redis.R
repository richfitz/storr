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
  dr <- driver_redis("storr")
  dr$flush()

  key <- "aaa"
  value <- "xxx"
  ns <- "objects"

  expect_that(dr$exists_key(key, ns), is_false())

  expect_that(dr$is_list(key, ns), is_false())
  expect_that(dr$length_list(key, ns), equals(0))
  expect_that(dr$get_hash_list(key, NULL, ns), equals(character(0)))

  expect_that(dr$get_hash_list(key, 1, ns),
              throws_error("1 is out of bounds"))
  expect_that(dr$get_hash_list(key, -1, ns),
              throws_error("-1 is out of bounds"))
  expect_that(dr$get_hash_list(key, c(1, 2), ns),
              throws_error("1, 2 is out of bounds"))

  expect_that(dr$set_key_hash_list(key, 1, value, ns),
              throws_error("Wrong type: expected list, recieved none"))
  expect_that(dr$set_key_hash_list(key, 1, -value, ns),
              throws_error("Wrong type: expected list, recieved none"))

  expect_that(dr$del_hash_list(key, ns), not(throws_error()))
})

test_that("Lists on objects", {
  dr <- driver_redis("storr")
  dr$flush()

  key <- "aaa"
  value <- "xxx"
  ns <- "objects"

  object_cache(dr)$set(key, 1:10)

  expect_that(dr$exists_key(key, ns), is_true())

  expect_that(dr$is_list(key, ns), is_false())
  expect_that(dr$length_list(key, ns), equals(0))
  expect_that(dr$get_hash_list(key, NULL, ns), equals(character(0)))

  expect_that(dr$get_hash_list(key, 1, ns),
              throws_error("1 is out of bounds"))
  expect_that(dr$get_hash_list(key, -1, ns),
              throws_error("-1 is out of bounds"))
  expect_that(dr$get_hash_list(key, c(1, 2), ns),
              throws_error("1, 2 is out of bounds"))

  expect_that(dr$set_key_hash_list(key, 1, value, ns),
              throws_error("Wrong type: expected list, recieved none"))
  expect_that(dr$set_key_hash_list(key, 1, -value, ns),
              throws_error("Wrong type: expected list, recieved none"))

  expect_that(dr$del_hash_list(key, ns), not(throws_error()))
})

test_that("Actual lists!", {
  dr <- driver_redis("storr")
  dr$flush()

  key <- "aaa"
  value <- 1:5
  hash <- vcapply(value, hash_object)
  ns <- "objects"

  dr$set_key_hash_list(key, NULL, hash, ns)

  expect_that(dr$is_list(key, ns), is_true())
  expect_that(dr$length_list(key, ns), equals(length(value)))
  expect_that(dr$get_hash_list(key, NULL, ns), equals(hash))

  expect_that(dr$get_hash_list(key, 1, ns),
              equals(hash[[1]]))
  expect_that(dr$get_hash_list(key, -1, ns),
              throws_error("-1 is out of bounds"))
  expect_that(dr$get_hash_list(key, c(1, 2), ns),
              equals(hash[c(1, 2)]))

  hash2 <- hash_object(6)
  expect_that(dr$set_key_hash_list(key, 1, hash2, ns),
              not(throws_error()))
  expect_that(dr$get_hash_list(key, 1, ns),
              equals(hash2))

  expect_that(dr$set_key_hash_list(key, -1, value, ns),
              throws_error("-1 is out of bounds"))

  expect_that(dr$del_hash_list(key, ns), not(throws_error()))
})
