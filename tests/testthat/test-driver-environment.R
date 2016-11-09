context("driver environment details")

test_that("hash", {
  hash_algos <- c("md5", "sha1")
  x <- runif(10)
  key <- "foo"
  hmd5 <- digest::digest(x, "md5")

  for (h in hash_algos) {
    e <- new.env(parent = emptyenv())
    st <- storr_environment(e, hash_algorithm = h)
    st$set(key, x)
    expect_equal(st$get(key), x)
    hash <- digest::digest(x, h)
    expect_equal(st$list_hashes(), hash)
    ## Sanity check
    expect_equal(hash == hmd5, h == "md5")

    h_other <- setdiff(hash_algos, h)[[1]]
    expect_error(storr_environment(e, hash_algorithm = h_other),
                 "Incompatible value for hash_algorithm")

    st$destroy()
  }
})
