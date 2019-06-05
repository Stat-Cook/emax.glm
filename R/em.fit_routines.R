#' Hessian routine
#' @inheritParams em.glm
#' @param b.list List of K-classes each entry bign a k length parameter vector,
#' @param class_probs  Matrix (n x K) of normalized class probabilities.
#' @param tol.1   Tolerance of the NR minimization.
#' @param debug  Boolen flag. Turn on to print optimization steps.
#' @param family GLM family to fit with.
#' @param maxiter   Maximum iterations of the NR methods for exiting before convergence.
#'
#' @export
em.glm_pracma_fit <- function(x, y, b.list, class_probs,  weight = c(1), K = 2, tol.1 = 1e-8, debug=T, family=poisson(), maxiter = Inf){
  u.list <- lapply(
    1:K,
    function(i) make.logLike(
      x, y,
      weight = weight, class_probs =class_probs[,i],
      family = family
    )
  )

  # Optimize each classes paramteres via Newton-Raphson
  for (i in 1:K){
    b.list[[i]] <- em.fit_pracma(
      u.list[[i]], b.list[[i]],
      x, y,
      weight = weight,
      class_probs = class_probs[,i],
      tol=tol.1, debug = debug,
      family = family, maxiter=maxiter
    )
  }

  return(b.list)
}

#' Numeric approximation routine
#'
#' @inheritParams em.glm_pracma_fit
#'
#' @export
em.glm_numeric_fit <- function(x, y, b.list, class_probs,  weight = c(1), K = 2, tol.1 = 1e-8, debug=T, family=poisson(), maxiter=Inf){
  for (i in 1:K){
    b.list[[i]] <- em.fit_numeric(
      b.list[[i]],
      x, y, weight = weight,
      class_probs = class_probs[,i],
      tol=tol.1, debug = debug,
      family = family, maxiter = maxiter
    )
  }

  return(b.list)
}
