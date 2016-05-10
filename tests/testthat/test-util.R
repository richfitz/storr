context("utils")

test_that("write_bin", {
  bytes <- as.raw(0:255)
  path <- tempfile()

  con <- file(path, "wb")
  on.exit({
    close(con)
    file.remove(path)
  })

  write_bin(bytes, con, 7L)

  close(con)
  on.exit(file.remove(path))

  expect_identical(readBin(path, raw(), 1000), bytes)
})
