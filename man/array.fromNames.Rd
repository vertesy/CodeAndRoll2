% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CodeAndRoll2.R
\name{array.fromNames}
\alias{array.fromNames}
\title{array.fromNames}
\usage{
array.fromNames(
  rowname_vec = 1:3,
  colname_vec = letters[1:2],
  z_name_vec = LETTERS[4:6],
  fill = NA
)
}
\arguments{
\item{rowname_vec}{A vector of names for rows, Default: 1:3}

\item{colname_vec}{A vector of names for columns, Default: letters\link{1:2}}

\item{z_name_vec}{A vector of names for Z dimension, Default: LETTERS\link{4:6}}

\item{fill}{The value to fill the new vector, Default: NA}
}
\description{
Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.
}
