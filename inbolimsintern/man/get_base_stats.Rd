% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic_functions.R
\name{get_base_stats}
\alias{get_base_stats}
\title{Krijg basisstatistieken}
\usage{
get_base_stats(x, keuze = c("n", "nas", "mean", "sd"))
}
\arguments{
\item{x}{vector with results}

\item{keuze}{character vector with stats to be calculated (n, nas, mean, sd, se, median, min, max, q.025, q.975,q.25,q.75
)}
}
\value{
named vector with the statistics
}
\description{
Krijg basisstatistieken
}
\examples{
x <- rnorm(100)
get_base_stats(x, keuze = c("n", "mean", "sd"))
}
