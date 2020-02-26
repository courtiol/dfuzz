test_that("test that output is generally correct 1", {
  vec <- c("banana", "blueberry", "APPLE", "apple", "aple", "Apple", "bonana")
  res <- fuzzy_pool(vec)
  ref <- list(messy = list(c("apple", "aple", "Apple"), c("banana", "bonana")),
              ok = c("blueberry", "APPLE"))
  expect_equal(res, ref)
})

test_that("test that pooling results do not contain duplicated items", {
  vec <- c("apple", "aple", "Apple", "appple", "oppple")
  res <- fuzzy_pool(vec)
  ref <- list(messy = list(c("apple", "aple", "Apple", "appple", "oppple")),
              ok = character(0))
  expect_equal(res, ref)
})

test_that("test that handles only messy", {
  vec <- c("apple", "aple", "Apple")
  res <- fuzzy_pool(vec)
  ref <- list(messy = list(c("apple", "aple", "Apple")),
              ok = character(0))
  expect_equal(res, ref)
})

test_that("test that handles only clean", {
  vec <- c("apple", "banana", "orange")
  res <- fuzzy_pool(vec)
  ref <- list(messy = list(),
              ok = c("apple", "banana", "orange"))
  expect_equal(res, ref)
})

test_that("test that handles length 1 vector", {
  vec <- "banana"
  res <- fuzzy_pool(vec)
  ref <- list(messy = list(), ok = "banana")
  expect_equal(res, ref)
})

test_that("test that handles NA", {
  vec <- c("banana", NA, "APPLE", "apple", NA, "Apple", "bonana")
  res <- fuzzy_pool(vec)
  ref <- list(messy = list(c("apple", "Apple"), c("banana", "bonana")),
              ok = c("APPLE"))
  expect_equal(res, ref)
})
