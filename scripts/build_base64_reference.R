#!/usr/bin/env Rscript
base64enc_reference <- function(x) {
  chartr("+/", "-_", base64enc::base64encode(charToRaw(x)))
}


rand_str_len <- function(n) {
  pos <- as.raw(32:126)
  rawToChar(sample(pos, n, replace = TRUE))
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, "", ...)
}


generate <- function() {
  set.seed(1)
  max_len <- 8
  str <- vcapply(rep(seq_len(max_len), each = 6), rand_str_len)
  val <- vcapply(str, base64enc_reference, USE.NAMES = FALSE)

  write.csv(data.frame(input = str, output = val, stringsAsFactors = FALSE),
            "tests/testthat/base64_reference.csv",
            row.names = FALSE)
}


generate()
