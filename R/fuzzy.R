#' Classify strings of characters according to their similarity
#'
#' There is no need for the user to call this function directly. This function
#' is used internally to create a list of strings of characters sorted according
#' to their similarity.
#'
#' It is possible to pass to this function arguments for
#' [`stringdist()`][`stringdist::stringdist`] to fine tune the computation of
#' the distance between strings.
#'
#' @param string_vec a vector of strings of characters
#' @param threshold the minimum distance between strings considered
#'   as OK
#' @param ... additional arguments for the function
#'   [`stringdist()`][`stringdist::stringdist`]
#'
#' @seealso `fuzzy_tidy()`, [`stringdist()`][`stringdist::stringdist`]
#'
#' @export
#'
#' @examples
#' test <- c("banana", "blueberry", "APPLE", "apple", "aple", "Apple", "bonana")
#' fuzzy_pool(test)
#'
fuzzy_pool <- function(string_vec, threshold = options("fuzzy_threshold")[[1]], ...) {

  ## check that inputs are correct:
  if (!is.null(dim(string_vec))) {
    stop("fuzzy_pool() needs string_vec to be a vector")
  }

  if (!is.character(string_vec) & !is.factor(string_vec)) {
    stop("fuzzy_pool() needs string_vec to be a vector")
  }

  if (!is.numeric(threshold)) {
    stop("fuzzy_pool() needs threshold to be a number")
  }

  ## unfactor:
  if (is.factor(string_vec)) {
    string_vec <- as.character(string_vec)
  }

  ## special case when nothing is messy for sure:
  if (length(string_vec) < 2 | length(unique(string_vec)) < 2) {
    return(list(messy = character(0), ok = unique(string_vec)))
  }

  ## comparisons of different strings (non duplicated):
  string_compared <- as.data.frame(t(utils::combn(unique(string_vec), 2L)), stringsAsFactors = FALSE)

  ## compute distance between strings:
  string_compared$dist <- stringdist::stringdist(string_compared$V1, string_compared$V2, ...)

  ## keep only strings for which distance is below threshold and discard the column with the distances:
  string_messy <- string_compared[string_compared$dist < threshold, 1:2]

  ## determine OK strings as those not messy:
  string_ok <- setdiff(string_vec, unique(unname(unlist(string_messy))))

  ## turn the data frame created into a clean list:
  list_messy <- split(string_messy, string_messy[[1]])
  out <- list(messy = lapply(list_messy, function(x) unique(unlist(x))),
              ok = string_ok)

  ## return:
  out
}


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

