% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{classes}
\alias{classes}
\title{Determining a class for each column vector in a data frame}
\usage{
classes(x)
}
\arguments{
\item{x}{a data frame}
}
\value{
a character vector
}
\description{
Determining a class for each column vector in a data frame
}
\examples{
b = data.frame(x = c(1,2,3),
          y = c(4,5,6),
          z = c(7,8,9),
          e = c("a", "b","d"))
classes(b)
# return
"numeric"   "numeric"   "numeric"   "character"
}
