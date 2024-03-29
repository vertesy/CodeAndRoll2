% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CodeAndRoll2.R
\name{translate}
\alias{translate}
\title{Translate values to a new set using a dictionary}
\usage{
translate(vec, oldvalues, newvalues)
}
\arguments{
\item{vec}{set of values where you want to replace}

\item{oldvalues}{oldvalues (from)}

\item{newvalues}{newvalues (to)}
}
\description{
Replaces a set of values in a vector with another set of values,
it translates your vector. Oldvalues and newvalues have to be 1-to-1
correspoding vectors.  'chartr("a-cX", "D-Fw", x) does the same as above
in theory, but it did not seem very robust regarding your input...'
}
\examples{
A <- 1:3
translate(vec = A, oldvalues = 2:3, newvalues = letters[1:2])

}
