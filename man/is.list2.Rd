% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CodeAndRoll2.R
\name{is.list2}
\alias{is.list2}
\title{Test if object is a list}
\usage{
is.list2(object)
}
\arguments{
\item{object}{Object to test.}
}
\description{
The 'is.list()' function fails on tibbles: it returns TRUE, as if it were a list. This distiguishes. Thaat's why we need this function.
}
\examples{
is.list2(list())
is.list2(dplyr::tibble())
}
