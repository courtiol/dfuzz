test_that("test that get_col() can use quoted name with nesting depth 0", {
  res <- get_col(iris, "Sepal.Length")
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})

test_that("test that get_col() can use non quoted name with nesting depth 0", {
  res <- get_col(iris, Sepal.Length)
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})

test_that("test that get_col() can use object with nesting depth 0", {
  b <- "Sepal.Length"

  res <- get_col(iris, b)
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})

test_that("test that get_col() can use quoted name with nesting depth 1", {
  get_col_nested1 <- function(.data, col) get_col(.data, col)

  res <- get_col_nested1(iris, "Sepal.Length")
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})

test_that("test that get_col() can use non quoted name with nesting depth 1", {
  get_col_nested1 <- function(.data, col) get_col(.data, {{col}})

  res <- get_col_nested1(iris, Sepal.Length)
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})

test_that("test that get_col() can use object with nesting depth 1", {
  b <- "Sepal.Length"

  get_col_nested1 <- function(.data, col) get_col(.data, {{col}})

  res <- get_col_nested1(iris, b)
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})

test_that("test that get_col() can use quoted name with nesting depth 3", {

  get_col_nested1 <- function(.data, col) get_col(.data, {{col}})
  get_col_nested2 <- function(.data, col) get_col_nested1(.data, {{col}})
  get_col_nested3 <- function(.data, col) get_col_nested2(.data, {{col}})

  res <- get_col_nested3(iris, "Sepal.Length")
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})

test_that("test that get_col() can use non quoted name with nesting depth 3", {
  get_col_nested1 <- function(.data, col) get_col(.data, {{col}})
  get_col_nested2 <- function(.data, col) get_col_nested1(.data, {{col}})
  get_col_nested3 <- function(.data, col) get_col_nested2(.data, {{col}})

  res <- get_col_nested3(iris, Sepal.Length)
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})

test_that("test that get_col() can use object with nesting depth 3", {
  b <- "Sepal.Length"

  get_col_nested1 <- function(.data, col) get_col(.data, {{col}})
  get_col_nested2 <- function(.data, col) get_col_nested1(.data, {{col}})
  get_col_nested3 <- function(.data, col) get_col_nested2(.data, {{col}})

  res <- get_col_nested3(iris, b)
  ref <- iris$Sepal.Length

  expect_equal(res, ref)
})
