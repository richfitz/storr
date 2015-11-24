## This requires
##   .driver_name: character(1)
##   .driver_create: function()
context(sprintf("driver-lists [%s]", .driver_name))

test_that("Lists on none", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  key <- "aaa"
  value <- "xxx"
  ns <- "objects"

  expect_that(dr$exists_key(key, ns), is_false())

  expect_that(dr$exists_list(key, ns), is_false())
  expect_that(dr$length_list(key, ns), equals(0))
  expect_that(dr$get_hash_list(key, NULL, ns), equals(character(0)))

  expect_that(dr$get_hash_list(key, 1, ns),
              throws_error("1 is out of bounds"))
  expect_that(dr$get_hash_list(key, -1, ns),
              throws_error("-1 is out of bounds"))
  expect_that(dr$get_hash_list(key, c(1, 2), ns),
              throws_error("1, 2 is out of bounds"))

  expect_that(dr$set_key_hash_list(key, 1, value, ns),
              throws_error("Wrong type: expected list, recieved"))
  expect_that(dr$set_key_hash_list(key, 1, -value, ns),
              throws_error("Wrong type: expected list, recieved"))

  expect_that(dr$del_hash_list(key, ns), not(throws_error()))
})

test_that("Lists on objects", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  key <- "aaa"
  value <- "xxx"
  ns <- "objects"

  storr(dr)$set(key, 1:10)

  expect_that(dr$exists_key(key, ns), is_true())

  expect_that(dr$exists_list(key, ns), is_false())
  expect_that(dr$length_list(key, ns), equals(0))
  expect_that(dr$get_hash_list(key, NULL, ns), equals(character(0)))

  expect_that(dr$get_hash_list(key, 1, ns),
              throws_error("1 is out of bounds"))
  expect_that(dr$get_hash_list(key, -1, ns),
              throws_error("-1 is out of bounds"))
  expect_that(dr$get_hash_list(key, c(1, 2), ns),
              throws_error("1, 2 is out of bounds"))

  expect_that(dr$set_key_hash_list(key, 1, value, ns),
              throws_error("Wrong type: expected list, recieved"))
  expect_that(dr$set_key_hash_list(key, 1, -value, ns),
              throws_error("Wrong type: expected list, recieved"))

  expect_that(dr$del_hash_list(key, ns), not(throws_error()))
})

test_that("Actual lists!", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  key <- "aaa"
  value <- 1:5
  hash <- vcapply(value, hash_object)
  ns <- "objects"

  dr$set_key_hash_list(key, NULL, hash, ns)

  expect_that(dr$exists_list(key, ns), is_true())
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
