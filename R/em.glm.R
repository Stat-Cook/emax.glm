#' Fit an Eexectation Maximization glm using the glm \emph{family} to define the link function.  Two methods of optimizationare included,
#' using direct numeric approximations and using the \emph{pracma} package to gind the Hessian and Jacobian of the log-likelihood.  The number of
#' competing models to be fit is set by \emph{K}.
#'
#' It is recommend users first call the \strong{em.small} command to run small warm up trails to explore the parameter space.
#'
#' @param x An \emph{n}-by-\emph{p} design matrix.
#' @param y A vetor of observation of length \emph{n}.
#' @param b.init The method to initialize EM parameters.  Built in methods are "random" and "fit" for pure white noise, and whie noise around GLM estiamtes.  Alternatively, pass a list of length K, each elelemtn consisting of a vector of length \emph{p}.  Users can also pass a zero-argument function to produce starting states.
#' @param weight A \emph{n} length vector of observation weight terms.  This is currently designed to be either the exposure for a Poisson model or the number of trials for a Logistic model.
#' @param K Number of EM classes to be fit.
#' @param family GLM family to fit.
#' @param method Control string.  Set to 'numeric' or 'pracma'.
#' @param param_errors Bool flag - set to TRUE to calculate parameter errors.
#' @param maxiter Maximum number of re-weighting rounds to do in fitting the EM model.  Primarily used to perform the 'small.em' warm-up routine.
#' @param maxiter.NR Maximum number of Newton-Raphson steps to take.
#' @param tol.1 Escape tolerance of the Newton-Raphson step.
#' @param tol.2 Escape tolerance of the re-weighting step.
#' @param noise Standard deviation of the white noise to be applied when generating random initial states.
#' @param debug Prints step-size in NR and re-weightign steps if TRUE.
#' @return An 'em.glm' object containing the class parameters, and class weights.
#' @export
em.glm <- function(
  x, y, b.init = "random,", weight = c(1), K = 2,
  family=poisson, method="numeric", maxiter=50, maxiter.NR = Inf,
  tol.1 = 1e-8, tol.2 = 1e-8, noise = 0.2, debug=F,
  param_errors = F){

  round <- 0
  q <- dim(x)[2]
  n <- dim(x)[1]

  if (!(method %in% c("numeric", "pracma"))) stop("Method not implemented, choose either 'numeric' or 'hessian")
  if (method == "pracma"){
    fit.function <- em.glm_pracma_fit
  }
  if (method == "numeric"){
    fit.function <- em.glm_numeric_fit
  }


  if (is.character(family))
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  dprob <- dprob.list[[family$family]](x=x, y=y, link=family$linkinv, weight=weight, log=FALSE)
  if (is.null(dprob)) {stop("Family not implemented!")}

  # Initalize parameters -------------------------------------

  if (class(b.init) == "function"){
    b.list <- b.init()
  }
  if (class(b.init) == "character"){
    if (all(b.init == "fit")){
      b.list <- init.fit(y = y, x = x, K = K, family=family, noise = noise, weight = weight)
    }

    if (all(b.init == "random")){
      b.list <- init.random(x, K = K, noise = noise)
    }
  }
  if (class(b.init) == "list"){
    b.list <- b.init
  }

  e.t <- 10

  # Initalize Weights ---------------------------------

  class_probs <- update_probabilities(dprob, b.list)

  #Carry out GLM optimizaton:

  ######

  while (e.t > tol.2 & round < maxiter){
    if (debug) {print(paste("Error in t:", e.t))}

    b.list <- fit.function(
      x=x, y=y,
      b.list=b.list, class_probs=class_probs,
      weight = weight, K=K,
      tol.1 = tol.1, debug=debug, family=family, maxiter = maxiter.NR
    )

    class_probs.new_values <- update_probabilities(dprob, b.list)

    # Check average shift in weights
    e.t <- mean(abs(class_probs - class_probs.new_values))

    class_probs <- class_probs.new_values

    round <- round + 1
  }

  result <- list(
    family = family,
    K = K,
    params = b.list,
    class_probs = class_probs,
    errors = lapply(1:K, function(...) rep("-", q))
  )

  if (param_errors){
    result$errors <- make_param_errors(b.list, x = x, y = y, weight = weight, family = family)
  }

  class(result) <- "em.glm"

  result$logLik <- logLik(result, x = x, y = y, weight = weight)

  result
}

#' @import stats
#' @import graphics
NULL
