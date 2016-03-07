##' Test that a driver passes all storr tests.  This page is only of
##' interest to people developing storr drivers; nothing here is
##' required for using storr.
##'
##' This will run through a suite of functions to test that a driver
##' is likely to behave itself.  As bugs are found they will be added
##' to the test suite to guard against regressions.
##'
##' The test suite is included in the package as
##' \code{system.file("spec", package="storr")}.
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
##' @param create A function with no arguments that when run will
##'   create a new driver instance.  Depending on your storage model,
##'   temporary directories, in-memory locations, or random-but-unique
##'   prefixes may help create isolated locations for the test (the
##'   tests assume that a storr created with \code{create} is entirely
##'   empty).
##'
##' @export
##' @examples
##' ## Testing the environment driver is nice and fast:
##' if (requireNamespace("testthat")) {
##'   test_driver(function() driver_environment())
##' }
##'
##' # To test things like the rds driver, I would run:
##' \dontrun{
##' if (requireNamespace("testthat")) {
##'   test_driver(function() driver_rds(tempfile()))
##' }
##' }
test_driver <- function(create) {
  if (!(is.function(create) && length(formals(create)) == 0L)) {
    stop("create must be a function of zero arguments")
  }
  loadNamespace("testthat")

  reporter <- testthat::get_reporter()
  standalone <- inherits(reporter, "StopReporter")
  if (standalone) {
    old_reporter <- reporter
    if (inherits(testthat::SummaryReporter, "refObjectGenerator")) {
      reporter <- testthat::SummaryReporter()
    } else {
      reporter <- testthat::SummaryReporter$new()
    }
    reporter$start_reporter()
    on.exit({
      reporter$end_reporter()
      testthat::set_reporter(old_reporter)
    })
  }

  files <- dir(system.file("spec", package="storr"),
               pattern="^test-", full.names=TRUE)
  env <- new.env(parent=environment(test_driver))
  env$.driver_name <- create()$type()
  env$.driver_create <- create
  res <- lapply(files, testthat::test_file, env=env,
                reporter=reporter, start_end_reporter=FALSE)

  if (standalone) {
    res <- do.call("rbind", lapply(res, as.data.frame))
    ntest <- sum(res$nb)
    nfail <- sum(res$failed)
    nerr <- sum(res$error)
    ok <- nfail == 0L && nerr == 0L
    msg <- sprintf("%s: %d %s, %d %s / %s tests total",
                   if (ok) "PASS" else "FAIL",
                   nerr, ngettext(nerr, "error", "errors"),
                   nfail, ngettext(nfail, "failure", "failures"),
                   ntest)
    if (ok) message(msg) else stop(msg, call.=FALSE)
    invisible(res)
  }
}
