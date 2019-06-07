#' Carry our the Newton-Raphson optimization of the parameters for given weights via the \strong{pracma} hessian,
#' @param u A 'model.loglike' function.
#' @param b The starting parameters.
#' @inheritParams make.logLike
#' @param debug Debugging flag - set to TRUE to output step-by-step change in parameter values.
#' @param maxiter Maximum number of NR steps to take.
#' @param tol The tolerance to repeat the Newton-Raphson optimization till.
#' @return The parameter values on convergence.
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#' u <- make.dpois
#' b <- c(1, 1, 1, 1)
#'
#' @export
em.fit_pracma <- function(u, b, x, y, class_probs, weight, tol=1e-8, debug = FALSE, family=poisson(), maxiter=Inf){

  e <- 10
  round <- 0
  rate <- y / weight

  while (e > tol & round < maxiter){
    hess <- pracma::hessian(u, b)

    tryCatch(
      hinv <- MASS::ginv(hess, tol=1e-8),
      error = function(e) print(b)
    )

    rho <- x %*% b
    score.vec <- t(x) %*% (class_probs * weight * (rate - family$linkinv(rho)))

    step <- hinv %*% score.vec
    e <- mean(abs(step))

    if (debug) {print(e)}

    b <- b - step
    round <- round + 1
  }

  return(b)
}


#' Carry our the Newton-Raphson optimization of the parameters for given weights via numeric approximations,
#' @inheritParams em.fit_pracma
#' @return The parameter values on convergence.
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#' u <- make.dpois(x, y)
#' b <- c(1, 1, 1, 1)
#' cp <- rep(1, 54)
#' em.fit_numeric(b = b, x=x, y=y, class_probs = cp)
#'
#' @export
em.fit_numeric <- function(b, x, y, class_probs, weight=c(1), tol=1e-8, debug = FALSE, family=poisson(), maxiter=Inf){

  e <- 10
  round <- 0
  rate <- y / weight

  while (e > tol & round < maxiter){

    rho <- x %*% b

    mu <- family$linkinv(rho)
    variance <-  class_probs * weight * family$variance(mu)[,1]

    hess <- -(t(x) %*% (variance * x))
    score.vec <- t(x) %*% (class_probs * weight * (rate - mu))

    tryCatch(
      {
        hinv <- MASS::ginv(hess, tol=1e-8)
        step <- hinv %*% score.vec
      },
      error = function(e) {
        step <- score.vec / diag(hess)
        warning("Matrix inversion failed ")
      }
    )

    step <- hinv %*% score.vec

    e <- mean(abs(step))

    if (debug) {print(e)}

    b <- b - step

    round <- round +1
  }

  b
}

