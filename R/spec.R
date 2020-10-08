##' Test that a driver passes all storr tests.  This page is only of
##' interest to people developing storr drivers; nothing here is
##' required for using storr.
##'
##' This will run through a suite of functions to test that a driver
##' is likely to behave itself.  As bugs are found they will be added
##' to the test suite to guard against regressions.
##'
##' The test suite is included in the package as
##' \code{system.file("spec", package = "storr")}.
##'
##' The procedure for each test block is:
##' \enumerate{
##' \item{Create a new driver by running \code{dr <- create()}.}
##' \item{Run a number of tests.}
##' \item{Destroy the driver by running \code{dr$destroy()}.}
##' }
##'
##' So before running this test suite, make sure this will not harm
##' any precious data!
##'
##' @title Test a storr driver
##'
##' @param create A function with one arguments that when run with
##'   \code{NULL} as the argument will create a new driver instance.
##'   It will also be called with a driver (of the same type) as an
##'   argument - in this case, you must create a new driver object
##'   pointing at the same underlying storage (see the examples).
##'   Depending on your storage model, temporary directories,
##'   in-memory locations, or random-but-unique prefixes may help
##'   create isolated locations for the test (the tests assume that a
##'   storr created with \code{create} is entirely empty).
##'
##' @export
##' @examples
##' ## Testing the environment driver is nice and fast:
##' if (requireNamespace("testthat")) {
##'   create_env <- function(dr = NULL, ...) {
##'     driver_environment(dr$envir, ...)
##'   }
##'   test_driver(create_env)
##' }
##'
##' # To test things like the rds driver, I would run:
##' \dontrun{
##' if (requireNamespace("testthat")) {
##'   create_rds <- function(dr = NULL) {
##'     driver_rds(if (is.null(dr)) tempfile() else dr$path)
##'   }
##'   test_driver(create_rds)
##' }
##' }
test_driver <- function(create) {
  loadNamespace("testthat")

  reporter <- testthat::get_reporter()
  standalone <- inherits(reporter, "StopReporter")
  if (standalone) {
    old_reporter <- reporter
    reporter <- testthat::SummaryReporter$new()
    reporter$start_reporter()
    on.exit({
      reporter$end_reporter()
      testthat::set_reporter(old_reporter)
    })
  }

  files <- dir(system.file("spec", package = "storr"),
               pattern = "^test-", full.names = TRUE)
  env <- new.env(parent = environment(test_driver))

  ## Need to get the reported type here, but also ensure that the
  ## driver cleans up correctly
  dr <- create()
  env$.driver_name <- dr$type()
  dr$destroy()

  env$.driver_create <- create

  ## This exists only until testthat version 3.0.x is released, when
  ## we'll just depend on the most recent copy.
  ## nocov start
  if (utils::packageVersion("testthat") < "2.99") {
    res <- lapply(files, testthat::test_file, env = env,
                  reporter = reporter, start_end_reporter = FALSE)

  } else {
    res <- lapply(files, testthat::test_file, env = env,
                  reporter = reporter)
  }
  ## nocov end

  df <- do.call("rbind", lapply(res, as.data.frame))

  test_driver_finish(df, standalone)
}


test_driver_finish <- function(df, standalone) {
  if (standalone) {
    ntest <- sum(df$nb)
    nfail <- sum(df$failed)
    nerr <- sum(df$error)
    ok <- nfail == 0L && nerr == 0L
    msg <- sprintf("%s: %d %s, %d %s / %s tests total",
                   if (ok) "PASS" else "FAIL",
                   nerr, ngettext(nerr, "error", "errors"),
                   nfail, ngettext(nfail, "failure", "failures"),
                   ntest)
    if (ok) {
      message(msg)
    } else {
      stop(msg, call. = FALSE)
    }
  }
  invisible(df)
}


dummy_test <- function() {
  testthat::expect_true(TRUE)
}
