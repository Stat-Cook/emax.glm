#' General Inforamtion Criteria function
#' @param em.glm An emax glm fit.
#' @inheritParams logLik.em.glm
#' @param alpha Scaling factor for inforamtion criteria (2 or ln(\emph{n}) for AIC and BIC respectively).
#'
#' @export
IC.em.glm <- function(em.glm, alpha){

  p <- dim(em.glm$class_probs)[2]
  K <- length(em.glm$params)

  return(K * p * alpha - 2 * em.glm$logLik)
}

#' Calcualte the BIC of the em.glm model
#'
#' @inheritParams  logLik.em.glm
#' @inheritParams stats::BIC
#' @return The BIC score of the model.
#'
#' @export
BIC.em.glm <- function(object, ...){

  n <- dim(object$class_probs)[1]

  return(IC.em.glm(object, log(n)))
}

#' Calcualte the AIC of the em.glm model
#'
#' @inheritParams  logLik.em.glm
#' @inheritParams stats::AIC
#' @return The AIC score of the model.
#'
#' @export
AIC.em.glm <- function(object, ..., k = 2){

  return(IC.em.glm(object, k))
}
