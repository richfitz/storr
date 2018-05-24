context("base64")

test_that("base64 reference", {
  ref <- read.csv("base64_reference.csv", stringsAsFactors = FALSE)

  expect_equal(encode64(ref$input), ref$output)
  expect_equal(decode64(ref$output), ref$input)
})

test_that("vector", {
  x <- encode64(letters)
  expect_equal(x, vcapply(letters, encode64, USE.NAMES = FALSE))
  expect_equal(decode64(x), letters)
})

test_that("padding", {
  ## This duplicates much of the code above.
  rand_str_len <- function(n) {
    pos <- as.raw(32:126)
    rawToChar(sample(pos, n, replace = TRUE))
  }

  err1 <- err2 <- err3 <- err4 <- character(0)

  for (len in 1:20) {
    for (r in 1:20) {
      s <- rand_str_len(len)
      t1 <- encode64(s, pad = TRUE)
      t2 <- encode64(s, pad = FALSE)
      if (!identical(decode64(t1), s)) {
        err1 <- c(err1, s)
      }
      if (!identical(decode64(t2), s)) {
        err2 <- c(err2, s)
      }

      n_pad <- 2 - (len - 1) %% 3
      if (substr(t1, nchar(t1) - n_pad + 1, nchar(t1) + 1) !=
          paste(rep("=", n_pad), collapse = "")) {
        err3 <- c(err3, s)
      }
      if (nchar(t1) != nchar(t2) + n_pad) {
        err4 <- c(err4, s)
      }
    }
  }
  expect_identical(length(err1), 0L)
  expect_identical(length(err2), 0L)
  expect_identical(length(err3), 0L)
  expect_identical(length(err4), 0L)
})

test_that("vector encode", {
  v <- c("x", "xx", "xxx")
  cmp <- vcapply(v, encode64, pad = TRUE, USE.NAMES = FALSE)
  expect_equal(encode64(v, pad = TRUE), cmp)
  expect_equal(encode64(v, pad = FALSE), sub("=+$", "", cmp))
})


test_that("invalid strings", {
  str <- "TWFyY2g= (conflicted copy)"
  expect_error(decode64(str), "is not base64")
  expect_identical(decode64(str, error = FALSE), NA_character_)

  v <- c(encode64("a"), "TWFyY2g= (conflicted copy)")
  expect_identical(decode64(v, error = FALSE), c("a", NA_character_))
})
