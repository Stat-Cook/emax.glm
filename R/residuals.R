#' Deviance residuals for an 'em.glm' object.
#' @param object An 'em.glm' object.
#' @inheritParams em.glm
#' @inheritParams stats::residuals
#' @param type Residual type - either deviance or Pearson's residuals.
#' @return An n length vector of residuals.
#'
#' @export
residuals.em.glm <- function(object, x, y, weight = c(1), type="deviance", ...){

  family <- object$family

  mu <- predict(object, x=x, y=y, weight=weight, type="rate")

  d.deviances <- list(
    "poisson" = function(y, mu, weight) dpois(y, y, log = T) - dpois(y, weight * mu, log = T),
    "binomial" = function(y, mu, weight) dbinom(y, weight, y / weight, log = T) - dbinom(y, weight, mu, log = T)
  )

  if (type == "deviance"){
    dev <- d.deviances[[family$family]]
    return(sign(y - weight * mu) * sqrt(2 * dev(y=y, mu=mu, weight = weight)))
  }
  if (type == "pearson"){
    return((y - weight * mu) / sqrt(weight * family$variance(mu)))
  }
  else{
    stop("Type not implemented.")
  }
}

#' Model deviance (calculated from deviance residuals)
#' @inheritParams residuals.em.glm
#' @inheritParams stats::deviance
#'
#' @export
deviance.em.glm <- function(object, x, y, weight = c(1), ...){
  resid <-residuals(object, x=x, y=y, weight=weight, type="deviance")

  return(sum(resid^2))
}
