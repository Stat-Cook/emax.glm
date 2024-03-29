% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.small.em.R
\name{summary.small.em}
\alias{summary.small.em}
\title{Summarize a small.em class}
\usage{
\method{summary}{small.em}(object, ...)
}
\arguments{
\item{object}{A small.em class}

\item{...}{additional arguments affecting the summary produced}
}
\value{
A data frame of log-likeliood, BIC and model index.
}
\description{
Summarize a small.em class
}
\examples{
x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
y <- warpbreaks$breaks

warm_up <- small.em(x = x, y = y, K = 2, b.init = "random", sample.size = 50)
summary(warm_up)

}
