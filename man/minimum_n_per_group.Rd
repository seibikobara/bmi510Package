% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{minimum_n_per_group}
\alias{minimum_n_per_group}
\title{Determine the minimum sample based on t distribution}
\usage{
minimum_n_per_group(d, power)
}
\arguments{
\item{d}{cohen's effect size}

\item{power}{power}
}
\value{
A integer
}
\description{
Determine the minimum sample based on t distribution
}
\examples{
minimum_n_per_group(1.0, power=0.8)
# returns
17 
}
