% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{n_incorrect}
\alias{n_incorrect}
\title{Number of incorrect classifications.}
\usage{
n_incorrect(m)
}
\arguments{
\item{m}{an object of class "clubprofit" produced by \code{club()}}
}
\value{
an integer.
}
\description{
Number of incorrect classifications.
}
\details{
Returns the number of observations which were
classified incorrectly by the model.
}
\examples{
mod <- club(rate ~ dose, data = caffeine)
n_incorrect(mod)
}
