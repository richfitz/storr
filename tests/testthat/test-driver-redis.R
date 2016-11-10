context("driver redis details")

test_that("hash", {
  skip_if_not_installed("redux")

  rand_str <- function() paste0(hash_object(Sys.time()), ":")
  con <- redux::hiredis()
  prefix <- rand_str()

  hash_algos <- c("md5", "sha1")
  x <- runif(10)
  key <- "foo"
  hmd5 <- digest::digest(x, "md5")
  h <- "md5"

  for (h in hash_algos) {
    st <- storr_redis_api(prefix, con, hash_algorithm = h)
    expect_equal(st$driver$hash_algorithm, h)
    st$set(key, x)
    expect_equal(st$get(key), x)
    hash <- digest::digest(x, h)
    expect_equal(st$list_hashes(), hash)
    ## Sanity check
    expect_equal(hash == hmd5, h == "md5")

    h_other <- setdiff(hash_algos, h)[[1]]
    expect_error(storr_redis_api(prefix, con, hash_algorithm = h_other),
                 "Incompatible value for hash_algorithm")

    st$destroy()
  }
})
