rand_str <- function(n = 16L, hex = TRUE) {
  if (hex) {
    paste(sample(as.raw(0:255), n, replace = TRUE), collapse = "")
  } else {
    rawToChar(sample(as.raw(32:126), n, replace = TRUE))
  }
}

skip_long_test <- function() {
  if (identical(Sys.getenv("STORR_RUN_LONG_TESTS"), "true")) {
    return(invisible(TRUE))
  }
  testthat::skip("Skipping long running test")
}

copy_to_tmp <- function(src) {
  path <- tempfile()
  dir.create(path)
  file.copy(src, path, recursive = TRUE)
  file.path(path, src)
}

has_postgres <- function(ctor) {
  !is.null(tryCatch(DBI::dbConnect(ctor()), error = function(e) NULL))
}
