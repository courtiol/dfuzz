% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{get_col}
\alias{get_col}
\title{Extract column from dataset}
\usage{
get_col(.data, col)
}
\arguments{
\item{.data}{A \code{data.frame} or \code{tbl}}

\item{col}{A column name}
}
\description{
This function extracts the column of a dataset irrespective of whether the
column is called by its quoted name, its non quotted name, or through an
object.
}
\details{
This function allow for writing code as if everything was relying on
non-standard evaluation (i.e. in the tidyverse style).
It is inspired from \code{\link[dplyr:pull]{pull()}}
}
\examples{
get_col(iris, "Sepal.Length")
get_col(iris, Sepal.Length)

}
\seealso{
\code{\link[=get_colname]{get_colname()}}
}
