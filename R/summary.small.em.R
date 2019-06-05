#' Summairze a small.em class
#' @param object A small.em class
#' @inheritParams summary.em.glm
#' @return A data frame of log-likeliood, BIC and model index.
#'
#' @export
summary.small.em <- function(object, ...){

  K <- length(object)
  ll <- as.numeric(sapply(1:K, function(i) object[[i]]$logLik))
  bic <- sapply(1:K, function(i) object[[i]]$bic)

  data.frame(
    index = 1:K,
    logLike = ll,
    bic = bic
  )
}
