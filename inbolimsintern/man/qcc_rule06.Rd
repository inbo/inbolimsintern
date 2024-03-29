% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elc_qcc_lims_shewhart_rules.R
\name{qcc_rule06}
\alias{qcc_rule06}
\title{Rule6: 4 out of 5 consecutive points are more than 1 sigma from the center line in the same direction}
\usage{
qcc_rule06(x, lcl_1s, ucl_1s, run = 5)
}
\arguments{
\item{x}{numeric vector of lab results}

\item{lcl_1s}{1-sigma lower limit}

\item{ucl_1s}{1-sigma upper limit}

\item{run}{violation if (run - 1) of run results meet the criterion}
}
\value{
TRUE if rule violation
}
\description{
Rule6: 4 out of 5 consecutive points are more than 1 sigma from the center line in the same direction
}
