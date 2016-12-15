skip_long_test <- function() {
  if (identical(Sys.getenv("STORR_RUN_LONG_TESTS"), "true")) {
    return(invisible(TRUE))
  }
  testthat::skip("Skipping long running test")
}
