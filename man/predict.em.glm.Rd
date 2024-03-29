% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.em.glm.R
\name{predict.em.glm}
\alias{predict.em.glm}
\title{Predict values from an 'em.glm' model.}
\usage{
\method{predict}{em.glm}(object, x, y, weight, type = "count", ...)
}
\arguments{
\item{object}{An em.glm fit object.}

\item{x}{An \emph{n}-by-\emph{p} design matrix.}

\item{y}{A vector of observation of length \emph{n}.}

\item{weight}{A \emph{n} length vector of observation weight terms.  This is currently designed to be either the exposure for a Poisson model or the number of trials for a Logistic model.}

\item{type}{Prediction type.  Currently can be 'count' for the weighted prediction, 'rate' for the expected rate or 'rho' for the linear predictor.}

\item{...}{optionally more fitted model objects.}
}
\value{
N-length vector of predicted terms.
}
\description{
Predict values from an 'em.glm' model.
}
\examples{
x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
y <- warpbreaks$breaks

m <- em.glm(x = x, y = y, K = 2, b.init = "random")

predict(m, x = x, y = y, weight = c(1))

}
