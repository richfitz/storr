context("drivers")

test_that("basic empty", {
  drivers <- create_drivers()
  on.exit(cleanup_drivers(drivers))
  for (dr in drivers) {
    expect_that(dr$list_hashes(), equals(character(0)))
    expect_that(dr$list_keys(),   equals(character(0)))

    expect_that(dr$get_value("aaa"),
                throws_error("hash 'aaa' not found"))
    expect_that(dr$get_hash("aaa"),
                throws_error("key 'aaa' not found"))

    expect_that(dr$del_hash("aaa"), is_false())
    expect_that(dr$del_key("aaa"),  is_false())

    expect_that(dr$exists_hash("aaa"), is_false())
    expect_that(dr$exists_key("aaa"), is_false())
  }
})

test_that("set", {
  drivers <- create_drivers()
  on.exit(cleanup_drivers(drivers))
  for (dr in drivers) {
    ## First, let's set some data to a hash
    d <- runif(100)

    dr$set_hash_value("aaa", d)
    expect_that(dr$get_value("aaa"), equals(d, tolerance=1e-15))

    ## Then, set a key to address that hash:
    dr$set_key_hash("bbb", "aaa")
    expect_that(dr$get_hash("bbb"), is_identical_to("aaa"))

    expect_that(dr$exists_hash("aaa"), is_true())
    expect_that(dr$exists_key("bbb"), is_true())

    ## Then delete the key:
    expect_that(dr$del_key("bbb"), is_true())
    expect_that(dr$get_hash("bbb"), throws_error("key 'bbb' not found"))

    ## And delete the hash:
    expect_that(dr$del_hash("aaa"), is_true())
    expect_that(dr$get_value("aaa"), throws_error("hash 'aaa' not found"))
  }
})

test_that("namespace", {
  drivers <- create_drivers()
  on.exit(cleanup_drivers(drivers))
  for (dr in drivers) {
    ## First, let's set some data to a hash
    d <- runif(100)

    dr$set_hash_value("aaa", d)
    expect_that(dr$get_value("aaa"), equals(d, tolerance=1e-15))

    ## Default namespace is objects:
    dr$set_key_hash("bbb", "aaa")
    expect_that(dr$get_hash("bbb", "objects"),
                is_identical_to("aaa"))
    expect_that(dr$get_hash("bbb"), is_identical_to("aaa"))

    expect_that(dr$list_keys(), equals("bbb"))
    expect_that(dr$list_keys("objects"), equals("bbb"))
    expect_that(dr$exists_key("bbb"), is_true())
    expect_that(dr$exists_key("bbb", "objects"), is_true())

    expect_that(dr$del_key("bbb", "objects"), is_true())
    expect_that(dr$exists_key("bbb", "objects"), is_false())

    ## Save into a different namespace:
    ns <- "another"
    dr$set_key_hash("ccc", "aaa", ns)
    expect_that(dr$get_hash("ccc", ns), is_identical_to("aaa"))

    expect_that(dr$list_keys(), equals(character(0)))
    expect_that(dr$list_keys(ns), equals("ccc"))
    expect_that(dr$exists_key("ccc"), is_false())
    expect_that(dr$exists_key("ccc", ns), is_true())

    expect_that(dr$del_key("ccc", ns), is_true())
    expect_that(dr$exists_key("ccc", ns), is_false())
  }
})
