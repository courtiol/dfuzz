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
