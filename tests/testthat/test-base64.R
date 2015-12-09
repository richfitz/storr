context("base64")

test_that("base64", {
  rand_str_len <- function(n) {
    pos <- as.raw(32:126)
    rawToChar(sample(pos, n, replace=TRUE))
  }
  if ("base64enc" %in% .packages(TRUE)) {
    cmp <- function(x) {
      ret <- base64enc::base64encode(charToRaw(x))
      gsub("+", "-", gsub("/", "_", ret, fixed=TRUE), fixed=TRUE)
    }
  } else {
    cmp <- encode64
  }

  err1 <- character(0)
  err2 <- character(0)

  for (len in 1:20) {
    for (r in 1:20) {
      s <- rand_str_len(len)
      t <- encode64(s)
      if (!identical(encode64(s), cmp(s))) {
        err1 <- c(err1, s)
      }
      if (!identical(decode64(t), s)) {
        err2 <- c(err2, s)
      }
    }
  }
  expect_identical(length(err1), 0L)
  expect_identical(length(err2), 0L)
})
