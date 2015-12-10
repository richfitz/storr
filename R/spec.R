## This is designed for helping with tests, in a fairly automated
## fashion.  Because I control the contents of the fest files, I can
## get away with a fairly bodgy parametrised testing approach.
test_driver <- function(name, create) {
  if (!(is.function(create) && length(formals(create)) == 0L)) {
    stop("create must be a function of zero arguments")
  }
  loadNamespace("testthat")

  reporter <- testthat::get_reporter()
  standalone <- inherits(reporter, "StopReporter")
  if (standalone) {
    old_reporter <- reporter
    reporter <- testthat::SummaryReporter()
    reporter$start_reporter()
    on.exit({
      reporter$end_reporter()
      testthat::set_reporter(old_reporter)
    })
  }

  files <- dir(system.file("spec", package="storr"),
               pattern="^test-", full.names=TRUE)
  env <- new.env(parent=environment(test_driver))
  env$.driver_name <- name
  env$.driver_create <- create
  res <- lapply(files, testthat::test_file, env=env,
                reporter=reporter, start_end_reporter=FALSE)

  if (standalone) {
    res <- do.call("rbind", lapply(res, as.data.frame))
    ntest <- sum(res$nb)
    nfail <- sum(res$failed) > 0
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
