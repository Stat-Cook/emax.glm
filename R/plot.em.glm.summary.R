#' Error bar plot of coefficients and errors to inspect class overlap.
#' @param x An EM summary object, from summary.em.glm
#' @param ...  Arguments to be passed to methods
#' @return An R plot with error bars.
#'
#' @export
plot.em.glm.summary <- function(x, ...){

  k <- length(x$params[[1]])
  vals <- c()

  for (i in x){
    vals <- c(vals, i$params)
  }
  col <- 0

  p <- plot(NULL, xlim=c(0,25), ylim=c(min(vals),max(vals)), ylab="y label", xlab="x lablel")
  for (i in x){
    p <- length(i$params)
    col <- col + 1
    y <- i$params
    error <- i$std_error
    points(1:p, y, col = col, pch = col)
    arrows(1:p, y0 = y-error, y1 = y + error, length=0.05, angle=90, code=3)
  }

}

#' Plot fit-parameters and errrors
#' @param x An em.glm fit object.
#' @param  known_params Prior estiamtes of fit parameters for comparison.
#' @param plot_type The plot type to diplay.  Defualts to lines, alternative include points.
#' @param add Boolean flag to decide if the plot should be added to an existing diplayed plot object or create a new axes.
#' @inheritParams plot.em.glm.summary
#'
#' @export
plot.em.glm <- function(x, known_params = NULL, plot_type = lines, add = FALSE, ...){

  k <- max(c(
    length(x$params[[1]]),
    length(known_params)
  ))
  vals <- unlist(x$params)

  if (!add){
    p <- plot(
      known_params,
      xlim = (c(0, k) + 0.5),
      ylim = c(min(vals), max(vals)),
      ylab = "Parameters",
      xlab = "Index"
    )
  }

  for (i in 1:x$K){
    x.plot <- 1:k
    y <- x$params[[i]]
    plot_type(x.plot, y, col = i, pch = i)

    error <- x$error[[i]]
    error.numeric <- sapply(error, is.numeric)

    if (any(error.numeric)){
      x <- x[error.numeric]
      y <- y[error.numeric]
      error <- error[error.numeric]
      arrows(x.plot, y0 = y - error, y1 = y + error, length=0.1, angle=90, code=3)
    }
  }
}

#' Probability plots for the K classes fit
#' @inheritParams  plot.em.glm.summary
#'
#' @export
plot_probabilities <- function(...){
  UseMethod("plot_probabilities")
}

#' Plot the class probabilities, both compared to data set index and as histogram.
#' @param class_probabilities Matrix of n x K class probabilities
#' @param ... Associated argumnets to be passed to plot::par function.
#'
#' @export
plot_probabilities.matrix <- function(class_probabilities, ...){
  K <- dim(class_probabilities)[2]

  par(mfrow = c(K, 2), ...)

    for (i in 1:K){
      plot(
        class_probabilities[, i],
        xlab = "index",
        ylim = c(0, 1),
        ylab = paste("P(Z = ", i, ")")
      )
      hist(
        class_probabilities[, i],
        main = paste("Histogram of P(Z = ", i, ")"),
        xlab = "P",
        xlim = c(0, 1)
      )
  }
}

#' Test Plot em.glm
#' @param em.glm An em.glm object.  From em.fit
#' @param ... Associated argumnets to be passed to plot::par function.
#'
#' @export
plot_probabilities.em.glm <- function(em.glm, ...){
  plot_probabilities.matrix(em.glm$class_probs, ...)
}
