#' Carry out several short EM fits to test for optimal starting locations.
#' @inheritParams em.glm
#' @param sample.size Number of cases to randomly select from the input data.
#' @param repeats Number of repetitions of the initialization to make.
#' @return A 'small.em' list containing the parameters, weights, log likelihood and BIC values.
#' @export
small.em <- function(x, y, b.init = "fit",
                     weight = c(1), K = 2,
                     maxiter=5, tol.1 = 1e-4, tol.2 = 1e-4,
                     noise = 0.2, sample.size = 500, repeats=5,
                     debug = F, family = "poisson", method="numeric", maxiter.NR = 20){

  len.weight <- length(weight)
  len.y <-length(y)

  if (len.weight != len.y){
    weight <- rep(weight, len.y)
  }

  n <- length(y)
  q <- sample(1:n, sample.size)

  small.list <- list()

  for (i in 1:repeats){
    tryCatch(
      {
        # Build a early terminating em.glm.
        small.list[[i]] <- em.glm(
          x[q,], y[q],
          weight = weight[q],
          maxiter = maxiter, K = K,
          tol.1 = tol.1, tol.2 = tol.2,
          b.init = b.init, noise = noise,
          debug = debug, family = family,
          method = method, maxiter.NR = maxiter.NR
        )

        small.list[[i]]$logLik <- logLik(small.list[[i]], y=y, x=x, weight=weight)
        small.list[[i]]$bic <- BIC(small.list[[i]])
      },
      error = function(e) small.list[[i]] <- "-"
    )
  }

  class(small.list) <- "small.em"

  small.list

}
