% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{cbind.fill}
\alias{cbind.fill}
\title{Retrieve data from UniProt using a query}
\usage{
\method{cbind}{fill}(...)
}
\arguments{
\item{...}{any number of R data objects.}

\item{fill}{R object to fill empty rows in columns below the max size. If unspecified, repeats input rows in the same way as cbind.}
}
\description{
Robust alternative to cbind that fills missing values and works on arbitrary data types.
}
\details{
Originally written for the row.r package by Craig Varrichio. Included here because the rowr package was discontinued. I use these functions in my packages
}
\examples{
cbind.fill(c(1,2,3),list(1,2,3),cbind(c(1,2,3)))
cbind.fill(rbind(1:2),rbind(3:4))
}
