#' Merge common elements from a list
#'
#' This function merges the commom elements from a list of depth 1.
#' The implementation is not very efficient, but it does the job for now.
#'
#' @param list A list
#'
#' @references [stackoverflow](https://stackoverflow.com/questions/47322126/merging-list-with-common-elements)
#'
#' @export
#'
#' @examples
#' test <- list(a = c(1, 2), b = c(1, 3), c = 4)
#' test
#' merge_common_list(test)
#'
merge_common_list <- function(list) {
  unique(sapply(list, function(x) {
    unique(unlist(list[sapply(list, function(y) any(x %in% y))]))
  }, simplify = FALSE))
}


#' Extract column name from dataset
#'
#' This function extracts a column name from a dataset irrespective of whether the
#' column is called by its quoted name, its non quotted name, or through an
#' object.
#'
#' @param .data A `data.frame` or `tbl`
#' @param col A column name
#'
#' @seealso [`get_col()`]
#'
#' @export
#'
#' @examples
#' get_colname(iris, "Sepal.Length")
#' get_colname(iris, Sepal.Length)
#' get_colname(iris, 1)
#'
get_colname <- function(.data, col) {
  cols <- rlang::set_names(seq_along(names(.data)), names(.data))
  col  <- rlang::eval_tidy(rlang::enquo(col), cols)
  if (is.numeric(col)) col <- names(cols)[col]
  col
}


#' Extract column from dataset
#'
#' This function extracts the column of a dataset irrespective of whether the
#' column is called by its quoted name, its non quotted name, or through an
#' object.
#'
#' This function allow for writing code as if everything was relying on
#' non-standard evaluation (i.e. in the tidyverse style).
#' It is inspired from [`pull()`][`dplyr::pull`]
#'
#' @inheritParams get_colname
#'
#' @seealso [`get_colname()`]
#'
#' @export
#'
#' @examples
#' get_col(iris, "Sepal.Length")
#' get_col(iris, Sepal.Length)
#'
get_col <- function(.data, col) {
  col <- get_colname(.data, {{col}})
  .data[[col]]
}



