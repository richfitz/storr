context("storr lists")

## TODO: test attributes
## TODO: test namespace

test_that("basic", {
  drivers <- create_drivers()
  on.exit(cleanup_drivers(drivers))

  for (dr in drivers) {
    value <- as.list(1:5)
    key <- "x"
    cache <- storr(dr)

    ## We'll set this as a list this way.

    cache$set_list(key, value, use_cache=FALSE)

    expect_that(cache$list(), equals(key))

    ## Get the whole list:
    expect_that(cache$get(key), equals(value))
    expect_that(ls(cache$envir), equals(hash_object(value)))
    cache$flush()
    expect_that(ls(cache$envir), equals(character(0)))

    ## Get an element from the list:
    expect_that(cache$get_list_element(key, 1), equals(value[[1]]))
    expect_that(cache$get_list_element(key, 5), equals(value[[5]]))

    expect_that(ls(cache$envir),
                equals_unsorted(c(hash_object(value[[1]]),
                                  hash_object(value[[5]]))))

    expect_that(cache$get_list(key), equals(value))

    expect_that(cache$get_list_element(key, 6),
                throws_error("Index 6 is out of bounds"))

    ## Get a bunch of elements:
    expect_that(cache$get_list_elements(key, 1:3),
                equals(value[1:3]))

    expect_that(cache$get_list_elements(key, NULL),
                equals(value))

    ## NULL here is attributes...
    expect_that(ls(cache$envir),
                equals_unsorted(c(vcapply(value, hash_object),
                                  hash_object(NULL))))

    ## Need to control what gets spat out of the return values here;
    ## lots of unnecessary noise.
    cache$set_list_element(key, 1, "a")
    expect_that(cache$get_list_element(key, 1), equals("a"))

    cmp <- value
    cmp[[1]] <- "a"

    expect_that(cache$list(), equals(key))

    expect_that(cache$get_list(key), equals(cmp))
    expect_that(cache$exists(key), is_true())
    expect_that(cache$driver$exists_key(key, "objects"), is_false())
    expect_that(cache$driver$exists_list(key, "objects"), is_true())

    expect_that(cache$get(key), equals(cmp))

    expect_that(cache$exists(key), is_true())
    expect_that(cache$driver$exists_key(key, "objects"), is_true())
    expect_that(cache$driver$exists_list(key, "objects"), is_true())
    expect_that(cache$list(), equals(key))

    cache$del(key)
    expect_that(cache$exists(key), is_false())
    expect_that(cache$driver$exists_key(key, "objects"), is_false())
    expect_that(cache$driver$exists_list(key, "objects"), is_false())
    expect_that(cache$list(), equals(character(0)))
  }
})

test_that("garbage collection", {
  drivers <- create_drivers()
  on.exit(cleanup_drivers(drivers))

  st <- storr(drivers$rds)

  st$set_list("mykey", 1:5)
  h <- st$list_hashes()
  st$gc()
  expect_that(st$list_hashes(), equals(h))

  st$set("another", 3)
  st$del("mykey")
  st$gc()
  expect_that(st$list_hashes(), equals(hash_object(3)))
})
