% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CodeAndRoll2.R
\name{movingAve}
\alias{movingAve}
\title{Moving / rolling average}
\usage{
movingAve(x, oneSide = 5)
}
\arguments{
\item{x}{A numeric vector.}

\item{oneSide}{The size of the moving window. Defaults to 5.}
}
\value{
A vector of the moving averages.
}
\description{
Calculates the moving / rolling average of a numeric vector.
}
\examples{
movingAve(1:5)

}
