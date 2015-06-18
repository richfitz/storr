context("github_release")

test_that("github_release", {
  expect_that(ls(github_release), equals(character(0)))

  read_csv <- function(...) {
    read.csv(..., stringsAsFactors=FALSE)
  }
  info <- github_release_storr_info("wcornwell/taxonlookup",
                                "plant_lookup.csv",
                                read_csv)

  expect_that(info, is_a("github_release_storr_info"))
  expect_that(info$name, equals("taxonlookup"))
  expect_that(info$path, is_a("character"))

  expect_that(info$path, equals(rappdirs::user_data_dir(info$name)))

  ## for testing use a temporary file
  path <- tempfile("storr")
  on.exit(unlink(path, recursive=TRUE))
  info <- github_release_storr_info("wcornwell/taxonlookup",
                                "plant_lookup.csv",
                                read_csv,
                                path=path)
  expect_that(info$path, equals(path))

  expect_that(file.exists(path), is_false())

  st <- github_release_storr(info)
  expect_that(file.exists(path), is_true())
  expect_that(st, is_a("storr"))
  expect_that(st$list(), equals(character(0)))

  skip_if_no_downloads()
  expect_that(github_release_storr_versions(info), equals(character(0)))

  tmp <- github_release_storr_versions(info, "github")
  expect_that(length(tmp), is_more_than(9))

  tmp <- github_release_storr_version_current(info)
  expect_that(numeric_version(tmp) >= numeric_version("0.3.1"),
              is_true())

  dat <- github_release_storr_get(info)

  expect_that(dat, is_a("data.frame"))
  expect_that(st$list(), equals(tmp))

  github_release_storr_del(info, tmp)
  expect_that(github_release_storr_versions(info, "local"),
              equals(character(0)))
  expect_that(file.exists(path), is_true())
  github_release_storr_del(info, NULL)
  expect_that(file.exists(path), is_false())
})
