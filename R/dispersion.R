#' Pearson-based dispersion  measurements of an 'em.glm' model.
#'
#' @param em.glm An 'em.glm' object.
#' @inheritParams em.glm
#'
#' @export
dispersion <- function(em.glm, x, y, weight){
  resid <- residuals(em.glm, x = x, y = y, weight = weight, type = "pearson")

  stat <- sum(resid^2)

  d <- dim(x)

  # TODO - Is this true?  Can you do underdispersion and overdispersion?
  overdisp_p_value <- pchisq(stat, d[1] - d[2], lower.tail = F)
  underdisp_p_value <- pchisq(stat, d[1] - d[2], lower.tail = T)

  list(
    "ssr" = stat,
    "pearson_dispersion" = stat / (d[1] - d[2]),
    "Over-dispersed pvalue" = overdisp_p_value,
    "Under-dispersed pvalue" = underdisp_p_value
  )
}


