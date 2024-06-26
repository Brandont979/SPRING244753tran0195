% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymaxlik.R
\name{mymaxlik}
\alias{mymaxlik}
\title{Max likelihood function}
\usage{
mymaxlik(lfun, x, param, ...)
}
\arguments{
\item{lfun}{the likelihood function}

\item{x}{vector of variables of the likelihood function}

\item{param}{vector of paremeters of the likelihood function}

\item{...}{additional arguments}
}
\value{
a list of the index of the maximum likelihood estimate, parameter value of the max likelihood estimate,
yi the log likelihood, and the slope of the log likelihood curve
}
\description{
Max likelihood function
}
\examples{
y = c(3,3,4,3,4,5,5,4)
logbin = function(x, param)
log(dbinom(x, prob = param, size = 20))
mymaxlik(lfun=logbin, x=y, param = seq(0, 1, length = 1000), main = "Binomial for n = 20")
}
