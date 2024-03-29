% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.em.glm.R
\name{summary.em.glm}
\alias{summary.em.glm}
\title{Summarize EM glm coefficients.}
\usage{
\method{summary}{em.glm}(object, ...)
}
\arguments{
\item{object}{An em.glm object}

\item{...}{additional arguments affecting the summary produced}
}
\value{
List of each classes coefficients and errors.
}
\description{
Summarize EM glm coefficients.
}
\examples{
x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
y <- warpbreaks$breaks

m <- em.glm(x = x, y = y, K = 2, b.init = "random")

summary(m)

}
