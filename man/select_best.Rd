% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_best.R
\name{select_best}
\alias{select_best}
\title{Select the best parameters from a set of results}
\usage{
select_best(small.em)

select_best(small.em)
}
\arguments{
\item{small.em}{A 'small.em' object}
}
\value{
The parameters of the best model, as judged by log-likelihood.
}
\description{
Select the best parameters from a set of results

Return the optimal model based on BIC scores
}
\examples{
x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
y <- warpbreaks$breaks

warm_up <- small.em(x = x, y = y, K = 2, b.init = "random", sample.size = 20)

select_best(warm_up)

}
