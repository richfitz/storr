context("copy")

test_that("copy list", {
  st <- storr_environment()

  src <- list(a = 1, b = 2)
  res <- st$import(src)
  expect_equal(sort(res[, "name"]), sort(names(src)))
  expect_equal(res[, "namespace"], rep("objects", 2L))

  expect_equal(st$get("a"), src$a)
  expect_equal(st$get("b"), src$b)
})

test_that("copy unsupported", {
  st <- storr_environment()
  expect_error(st$import(runif(10)),
               "Invalid type for src")
})

test_that("as.list", {
  st <- storr_environment()
  src <- list(a = runif(10), b = runif(20))
  res <- st$import(src)
  dat <- as.list(st)
  expect_equal(dat, src[names(dat)])
})

test_that("unknown types", {
  st <- storr_environment()

  src <- list(a = 1, b = 2)
  expect_error(
    st$import(unlist(src), list = names(src)),
    "Invalid type for src; can't 'get' from objects of type numeric")

  res <- st$import(src)

  expect_error(
    st$export(numeric(), list = names(res)),
    "Invalid type for dest; can't 'set' into objects of type numeric")
})

test_that("copy with different hash algorithms", {
  st_md5 <- storr_environment(hash_algorithm = "md5")
  st_sha <- storr_environment(hash_algorithm = "sha1")

  x <- runif(10)
  h <- st_md5$set("x", x)

  res <- st_sha$import(st_md5)
  expect_equal(unname(res[, "name"]), "x")
  expect_equal(unname(res[, "namespace"]), "objects")
  expect_equal(st_sha$get("x"), x)
  expect_false(st_sha$get_hash("x") == h)
})

test_that("copy multiple namespaces", {
  st1 <- storr_environment()
  st1$set("a", runif(10))
  st1$set("b", runif(10))
  st1$set("x", runif(10), "other")

  st2 <- storr_environment()
  dat <- st2$import(st1, namespace = NULL)

  expect_equal(nrow(dat), 3L)
  expect_equal(st1$list_hashes(), st2$list_hashes())
  expect_equal(st1$list_namespaces(), st2$list_namespaces())
  expect_equal(lapply(st1$list_namespaces(), st1$list),
               lapply(st2$list_namespaces(), st2$list))
})

test_that("named export", {
  st1 <- storr_environment()
  st1$set("a", runif(10))
  st1$set("b", runif(10))
  st1$set("x", runif(10), "other")

  st2 <- storr_environment()
  res <- st2$import(st1, c(A = "a", B = "b"))
  expect_equal(res[, "name"], c("A", "B"))
  expect_equal(st2$get("A"), st1$get("a"))
  expect_equal(st2$get("B"), st1$get("b"))

  st3 <- storr_environment()
  st1$export(st3, c(A = "a", B = "b"))
  expect_equal(res[, "name"], c("A", "B"))
  expect_equal(st3$get("A"), st1$get("a"))
  expect_equal(st3$get("B"), st1$get("b"))

  st4 <- storr_environment()
  res <- st4$import(st1, namespace = c(import = "objects"))
  expect_equal(res[, "namespace"], rep("import", 2L))
  expect_equal(sort(res[, "name"]), st1$list())
})

test_that("missing values on export", {
  st1 <- storr_environment()
  st1$set("a", runif(10))
  st1$set("b", runif(10))

  st2 <- storr_environment()
  expect_error(st2$import(st1, c("a", "b", "c")),
               "Missing values; can't copy")
  expect_equal(st2$list_hashes(), character(0))
  expect_equal(st2$list(), character(0))

  st2 <- storr_environment()
  res <- st2$import(st1, c("a", "b", "c"), skip_missing = TRUE)
  expect_equal(nrow(res), 2L)
  expect_equal(st2$list(), c("a", "b"))
})

test_that("can't export multiple namespaces to a list", {
  st1 <- storr_environment()
  st1$set("a", runif(10))
  st1$set("b", runif(10))
  st1$set("x", runif(10), "other")
  expect_error(st1$export(list(), namespace = NULL),
               "both dest and src must be storrs")
  expect_error(st1$export(new.env(parent = emptyenv()), namespace = NULL),
               "both dest and src must be storrs")
  expect_error(as.list(st1, namespace = NULL),
               "both dest and src must be storrs")
})

test_that("effect of default namespaces", {
  st1 <- storr_environment(default_namespace = "ns1")
  st2 <- storr_environment(default_namespace = "ns2")

  st1$set("a", runif(10))
  st2$set("b", runif(10))

  ## Both import and export default to import/export from the *self*
  ## default namespace, so this is a noop:
  res <- st1$import(st2)
  expect_equal(nrow(res), 0L)

  ## On the other hand, this will cause the contents of st1's ns1 to
  ## be copied over:
  st1$export(st2)
  expect_equal(st2$get("a", "ns1"), st1$get("a"))
})

test_that("null namespace on import of non-storr", {
  st <- storr_environment()
  expect_error(st$import(list(a = 1, b = 2), NULL, NULL),
               "namespace can't be NULL")
})
