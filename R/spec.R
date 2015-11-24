## This is designed for helping with tests, in a fairly automated
## fashion.  Because I control the contents of the fest files, I can
## get away with a fairly bodgy parametrised testing approach.
test_driver <- function(name, create) {
  if (!(is.function(create) && length(formals(create)) == 0L)) {
    stop("create must be a function of zero arguments")
  }
  loadNamespace("testthat")
  reporter <- testthat::get_reporter()

  files <- dir(system.file("spec", package="storr"),
               pattern="^test-", full.names=TRUE)
  for (f in files) {
    env <- new.env(parent=environment(test_driver))
    env$.driver_name <- name
    env$.driver_create <- create
    testthat::test_file(f, env=env, reporter=reporter, start_end_reporter=FALSE)
  }
}

equals_unsorted <- function(expected, ...) {
  eq <- testthat::equals(sort(expected), ...)
  function(actual) {
    eq(sort(actual))
  }
}
