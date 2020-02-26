#' Create a template table for tidying a text-column
#'
#' @inheritParams fuzzy_pool
#' @param .data A `data.frame` or `tbl`
#' @param stringvar The name of the column to be tidy (quoted or not)
#' @param .listpool An optional list provided by [`fuzzy_pool()`]
#'
#' @seealso [`stringdist()`][`stringdist::stringdist`]
#'
#' @export
#'
#' @examples
#' test_df <- data.frame(fruit = c("banana", "blueberry", "limon", "pinapple",
#'                                 "apple", "aple", "Apple", "bonana"))
#' fuzzy_template <- fuzzy_match(test_df, fruit)
#' fuzzy_template
#'
fuzzy_match <- function(.data, stringvar, threshold = options("fuzzy_threshold")[[1]], .listpool = NULL, ...) {

  ## retrieve the column as a vector:
  if (is.character(substitute(stringvar))) stringvar <- as.symbol(stringvar)
  stringvar <- substitute(stringvar)
  string_vec <- as.character(eval(stringvar, .data, parent.frame()))

  ## compute the fuzzy matches::
  if (is.null(.listpool)) {
    list_pools <- fuzzy_pool(string_vec, threshold, ...)
  } else {
    list_pools <- .listpool
  }

  ## compute column number:
  lengths <- unlist(lapply(list_pools$messy, length))
  max_possible <- ifelse(is.null(lengths), 0, max(lengths))

  ## turn the list created into a clean data frame:
  index_to_do <- seq_len(max_possible)
  list_pools_short <- lapply(list_pools$messy, function(x) x[index_to_do])
  df_matches <- as.data.frame(do.call("rbind", list_pools_short), stringsAsFactors = FALSE)
  if (max_possible > 0) {
    colnames(df_matches) <- paste0("syn_", seq_len(max_possible))
  }
  rownames(df_matches) <- NULL
  out <- cbind(selected = df_matches$syn_1, df_matches, stringsAsFactors = FALSE)

  ## turn into a tibble:
  class(out) <- c("tbl_df", "tbl", "data.frame")

  ## return:
  out
}
