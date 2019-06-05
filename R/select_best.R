#' Select the best parameters from a set of results
#'
#' @return The parameters of the best model, as judged by log-likelihood.
#' @export
select_best <- function(x, ...){
  UseMethod("select_best")
}

#' Return the optimal model based on BIC scores
#' @param small.em  A 'small.em' object
#' @export
select_best <- function(small.em){
  s <- summary(small.em)
  ordered <- s[order(s$bic), ]
  idx <- ordered$index[[1]]
  small.em[[idx]]$params
}
