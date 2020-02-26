#' Tidy a messy column of strings of characters in a table
#'
#' This function takes a column of strings of characters and attempts to tidy
#' it.
#'
#' The function adds three new column in the dataset. For example, if you target
#' column is called fruit, it will add:
#'
#' - `fruit.clean` a column containing only the elements that were considered as
#' OK
#' - `fruit.cleaned` a column containing only the elements substituting the
#' one considered as messy
#' - `fruit.tidy` a column with all the elements after
#' cleaning, and thus the proposition as replacement for your original column
#'
#' If the tidying does not satisfy you, think of adjusting the argument
#' `threshold` either direclty when calling the function or by
#' setting a general option for the package using
#' `options("threshold" = X)` with X the number of your choice. You
#' can also improve the tidying by providing fine-tunning argument to the
#' underlying workhorse [`stringdist()`][`stringdist::stringdist`] using the ...
#' argument.
#'
#' You can usea template created with `fuzzy_match`() to control how the messy strings
#' are being tidy.
#'
#' @inheritParams fuzzy_match
#' @param template A lookup table created with [`fuzzy_match()`] (optional)
#'
#' @seealso `fuzzy_pool()`, `fuzzy_match()`, [`stringdist()`][`stringdist::stringdist`]
#'
#' @export
#'
#' @examples
#' test_df <- data.frame(fruit = c("banana", "blueberry", "limon", "pinapple",
#'                                 "apple", "aple", "Apple", "bonana"),
#'                       number = 1:8)
#' fuzzy_template <- fuzzy_match(test_df, fruit)
#' fuzzy_template
#' fuzzy_tidy(test_df, "fruit", fuzzy_template)
#' fuzzy_tidy(test_df, "fruit")
#'
fuzzy_tidy <- function(.data, stringvar, template = NULL, threshold = options("fuzzy_threshold")[[1]], ...) {

  ## extract column name:
  name_var <- as.character(substitute(stringvar))

  ## retrieve the column as a vector:
  if (is.character(substitute(stringvar))) stringvar <- as.symbol(stringvar)
  stringvar <- substitute(stringvar)
  string_vec <- as.character(eval(stringvar, .data, parent.frame()))

  ## compute the fuzzy matches:
  list_pools <- fuzzy_pool(string_vec, threshold, ...)

  ## build lookup table if missing:
  if (is.null(template)) {
    template <- fuzzy_match(.data, stringvar, threshold, .listpool = list_pools, ...)
  }

  ## convert to character if needed:
  if (is.factor(template$selected)) {
    template$selected <- as.character(template$selected)
  }

  ## merge OK strings:
  ok <- string_vec %in% list_pools$ok
  .data$.clean[ok] <- string_vec[ok]

  ## merge cleaned strings:
  messy_index <- (match(string_vec, as.vector(t(template))) - 1) %/% ncol(template) + 1 ## to check throroughly
  .data$.cleaned <- template$selected[messy_index]

  ## create tidy column:
  .data$.tidy <- .data$.clean
  .data$.tidy[is.na(.data$.tidy)] <- .data$.cleaned[is.na(.data$.tidy)]

  ## rename columns:
  colnames(.data)[colnames(.data) == ".clean"] <- paste0(name_var, ".clean")
  colnames(.data)[colnames(.data) == ".cleaned"] <- paste0(name_var, ".cleaned")
  colnames(.data)[colnames(.data) == ".tidy"] <- paste0(name_var, ".tidy")

  ## turn into a tibble:
  class(.data) <- c("tbl_df", "tbl", "data.frame")

  ## output:
  .data
}
