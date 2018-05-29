## TODO: check that *updating* a key works in the spec tests (i.e., hit set_hash(key, namespace, hash)
## This requires
##   .driver_name: character(1)
##   .driver_create: function()
testthat::context(sprintf("drivers [%s]", .driver_name))


testthat::test_that("basic (empty)", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  testthat::expect_equal(dr$type(), .driver_name)

  ns <- "objects"
  testthat::expect_equal(dr$list_namespaces(), character(0))
  testthat::expect_equal(dr$list_hashes(), character(0))
  testthat::expect_equal(dr$list_keys(ns), character(0))

  h <- "aaa"
  k <- "bbb"
  ns <- "ns"

  testthat::expect_false(dr$exists_object(h))
  testthat::expect_false(dr$exists_hash(k, ns))

  testthat::expect_false(dr$del_object(h))
  testthat::expect_false(dr$del_hash(h, ns))
})


testthat::test_that("set", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  ## First, let's set some data to a hash
  d <- runif(100)
  h <- st$hash_object(d)
  k <- "bbb"
  ns <- "ns"

  if (storr_traits(dr$traits)[["accept"]] == "object") {
    dr$set_object(h, d)
  } else {
    dr$set_object(h, st$serialize_object(d))
  }
  testthat::expect_equal(dr$get_object(h), d, tolerance = 1e-15)

  ## Then, set a key to address that hash:
  dr$set_hash(k, ns, h)
  testthat::expect_identical(dr$get_hash(k, ns), h)

  ## Check that *updating* a hash works.
  h2 <- st$hash_object(h)
  dr$set_hash(k, ns, h2)
  testthat::expect_identical(dr$get_hash(k, ns), h2)

  testthat::expect_true(dr$exists_object(h))
  testthat::expect_true(dr$exists_hash(k, ns))

  ## Then delete the key:
  testthat::expect_true(dr$del_hash(k, ns))
  testthat::expect_false(dr$del_hash(k, ns))
  testthat::expect_false(dr$exists_hash(k, ns))

  ## And delete the hash:
  testthat::expect_true(dr$del_object(h))
  testthat::expect_false(dr$del_object(h))
  testthat::expect_false(dr$exists_object(h))
})


testthat::test_that("namespace", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  ## First, let's set some data to a hash
  d <- runif(100)
  h <- st$hash_object(d)
  k <- "bbb"
  ns <- "ns"

  if (storr_traits(dr$traits)[["accept"]] == "object") {
    dr$set_object(h, d)
  } else {
    dr$set_object(h, st$serialize_object(d))
  }
  testthat::expect_equal(dr$get_object(h), d, tolerance = 1e-15)
  testthat::expect_true(dr$exists_object(h))

  dr$set_hash(k, ns, h)
  testthat::expect_identical(dr$get_hash(k, ns), h)

  testthat::expect_identical(dr$list_keys(ns), k)
  testthat::expect_true(dr$exists_hash(k, ns))

  testthat::expect_true(dr$del_hash(k, ns))
  testthat::expect_false(dr$exists_hash(k, ns))
  testthat::expect_true(dr$exists_object(h))

  ## Save into a different namespace:
  ns2 <- "another"
  k2 <- "ccc"

  dr$set_hash(k2, ns2, h)
  testthat::expect_true(dr$exists_hash(k2, ns2))
  testthat::expect_identical(dr$get_hash(k2, ns2), h)

  testthat::expect_identical(dr$list_keys(ns), character(0))
  testthat::expect_identical(dr$list_keys(ns2), "ccc")
  testthat::expect_identical(dr$list_hashes(), h)

  testthat::expect_true(dr$del_hash(k2, ns2))
  testthat::expect_false(dr$exists_hash(k2, ns2))
})


testthat::test_that("traits: throw_missing", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  if (isTRUE(dr$traits[["throw_missing"]])) {
    str <- paste(sample(letters), collapse = "")
    testthat::expect_error(dr$get_hash(str, "objects"))
    testthat::expect_error(dr$get_object(st$hash_object(str)))
  } else {
    dummy_test()
  }
})


testthat::test_that("exists (vector input)", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  st <- storr(dr)

  aaa <- runif(10)
  bbb <- runif(20)
  st$set("aaa", aaa)
  st$set("bbb", bbb)

  ## From one namespace:
  testthat::expect_equal(dr$exists_hash(character(0), "objects"), logical(0))
  testthat::expect_equal(dr$exists_hash("aaa", "objects"), TRUE)
  testthat::expect_equal(dr$exists_hash(c("aaa", "bbb"), "objects"),
                         c(TRUE, TRUE))
  testthat::expect_equal(dr$exists_hash(c("aaa", "bbb", "ccc"), "objects"),
               c(TRUE, TRUE, FALSE))

  ## Empty namespace:
  testthat::expect_equal(dr$exists_hash("aaa", character(0)), logical(0))

  ## Multiple namespaces, single key:
  testthat::expect_equal(dr$exists_hash("aaa", c("objects", "another")),
                         c(TRUE, FALSE))

  ## Multiple namespaces, multiple keys:
  testthat::expect_equal(dr$exists_hash(c("aaa", "bbb", "aaa"),
                              c("objects", "another", "another")),
               c(TRUE, FALSE, FALSE))

  h <- st$list_hashes()
  testthat::expect_equal(dr$exists_object(h), c(TRUE, TRUE))
  testthat::expect_equal(dr$exists_object(character(0)), logical(0))
})


testthat::test_that("del (vector input)", {
  dr <- .driver_create()
  on.exit(dr$destroy())
  st <- storr(dr)

  aaa <- runif(10)
  bbb <- runif(20)
  st$set("aaa", aaa)
  st$set("bbb", bbb)

  ## remove two at once:
  testthat::expect_equal(dr$del_hash(c("aaa", "bbb"), "objects"),
                         c(TRUE, TRUE))
  testthat::expect_equal(dr$del_hash(c("aaa", "bbb"), "objects"),
                         c(FALSE, FALSE))

  ## Remove none:
  testthat::expect_equal(dr$del_hash(character(0), "objects"), logical(0))
  testthat::expect_equal(dr$del_hash("aaa", character(0)), logical(0))

  ## Mixed removal:
  st$set("aaa", aaa)
  testthat::expect_equal(dr$del_hash(c("aaa", "bbb"), "objects"),
                         c(TRUE, FALSE))
  st$set("bbb", bbb)
  testthat::expect_equal(dr$del_hash(c("aaa", "bbb"), "objects"),
                         c(FALSE, TRUE))

  ## Remove the actual data:
  h <- st$list_hashes()
  testthat::expect_equal(dr$del_object(character(0)), logical(0))
  testthat::expect_equal(dr$del_object(h), c(TRUE, TRUE))
  testthat::expect_equal(dr$del_object(h), c(FALSE, FALSE))

  ## Mixed removal:
  st$set("aaa", aaa, use_cache = FALSE)
  ha <- st$get_hash("aaa")
  testthat::expect_equal(dr$del_object(h), h == ha)
  st$set("bbb", bbb, use_cache = FALSE)
  testthat::expect_equal(dr$del_object(h), h != ha)
})


testthat::test_that("mget_object", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  if (is.null(dr$mget_object)) {
    dummy_test()
  } else {
    st <- storr(dr)
    h1 <- st$set("a", 1)
    h2 <- st$set("b", 2)
    testthat::expect_equal(dr$mget_object(character(0)), list())
    testthat::expect_equal(dr$mget_object(h1), list(1))
    testthat::expect_equal(dr$mget_object(c(h1, h2)), list(1, 2))
    testthat::expect_equal(dr$mget_object("aaaaaa"), list(NULL))
    testthat::expect_equal(dr$mget_object(c(h1, "aaaaaa")), list(1, NULL))
  }
})


testthat::test_that("mget_hash", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  if (is.null(dr$mget_hash)) {
    dummy_test()
  } else {
    st <- storr(dr)
    h1 <- st$set("a", 1)
    h2 <- st$set("b", 2)
    h3 <- st$set("c", 3, "other")
    testthat::expect_equal(dr$mget_hash(character(0), "objects"), character(0))
    testthat::expect_equal(dr$mget_hash("a", character(0)), character(0))

    testthat::expect_equal(dr$mget_hash("a", "objects"), h1)
    testthat::expect_equal(dr$mget_hash("b", "objects"), h2)
    testthat::expect_equal(dr$mget_hash("c", "objects"), NA_character_)

    testthat::expect_equal(dr$mget_hash(c("a", "b"), "objects"),
                           c(h1, h2))
    testthat::expect_equal(dr$mget_hash(c("a", "c", "b"), "objects"),
                           c(h1, NA, h2))

    testthat::expect_equal(dr$mget_hash(c("a", "b", "c"),
                                        c("objects", "objects", "other")),
                           c(h1, h2, h3))

    testthat::expect_equal(dr$mget_hash("a", c("objects", "other")),
                           c(h1, NA_character_))

    ## duplicate keys:
    testthat::expect_equal(dr$mget_hash(c("a", "b", "a", "b", "c"), "objects"),
                           c(h1, h2, h1, h2, NA))
  }
})


testthat::test_that("mset_object", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  if (is.null(dr$mset_object)) {
    dummy_test()
  } else {
    st <- storr(dr)

    ## Lots of faff here:
    a <- runif(10)
    b <- runif(3)
    ha <- st$hash_object(a)
    hb <- st$hash_object(b)
    if (storr_traits(dr$traits)[["accept"]] == "object") {
      sa <- a
      sb <- b
    } else {
      sa <- st$serialize_object(a)
      sb <- st$serialize_object(b)
    }

    ## Test the empty one
    testthat::expect_silent(dr$mset_object(list(), character(0)))
    testthat::expect_equal(dr$list_hashes(), character(0))

    ## Actually insert some data
    dr$mset_object(c(ha, hb), list(sa, sb))
    testthat::expect_equal(sort(dr$list_hashes()),
                           sort(c(ha, hb)))

    testthat::expect_equal(dr$get_object(ha), a)
    testthat::expect_equal(dr$get_object(hb), b)
  }
})


testthat::test_that("mset_hash", {
  dr <- .driver_create()
  on.exit(dr$destroy())

  if (is.null(dr$mset_hash)) {
    dummy_test()
  } else {
    ns <- "objects"
    testthat::expect_silent(dr$mset_hash(character(0), ns, character(0)))
    testthat::expect_silent(dr$mset_hash("a", character(0), character(0)))
    testthat::expect_equal(dr$list_keys(ns), character(0))

    ## Then start actually setting things:
    dr$mset_hash("a", ns, "aaa")
    testthat::expect_equal(dr$get_hash("a", ns), "aaa")

    dr$mset_hash(c("a", "b"), ns, c("AAA", "BBB"))
    testthat::expect_equal(dr$get_hash("a", ns), "AAA")
    testthat::expect_equal(dr$get_hash("b", ns), "BBB")

    ## Duplicate keys; take the last one:
    dr$mset_hash(c("x", "x"), ns, c("aaa", "bbb"))
    testthat::expect_equal(dr$get_hash("x", ns), "bbb")

    dr$mset_hash(c("x", "x"), ns, c("bbb", "aaa"))
    testthat::expect_equal(dr$get_hash("x", ns), "aaa")
  }
})
