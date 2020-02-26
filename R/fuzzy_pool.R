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
