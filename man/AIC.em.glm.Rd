% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BIC.em.glm.R
\name{AIC.em.glm}
\alias{AIC.em.glm}
\title{Calculate the AIC of the em.glm model}
\usage{
\method{AIC}{em.glm}(object, ..., k = 2)
}
\arguments{
\item{object}{A 'em.glm' class returned by the em.glm function.}

\item{...}{optionally more fitted model objects.}

\item{k}{numeric, the \emph{penalty} per parameter to be used; the
    default \code{k = 2} is the classical AIC.}
}
\value{
The AIC score of the model.
}
\description{
Calculate the AIC of the em.glm model
}
\examples{
y <- c(AirPassengers)
n <- length(y)
x <- as.matrix(rep(1, n))
m <- em.glm(x = x, y = y, K = 2, b.init = "random")
AIC(m)

}
