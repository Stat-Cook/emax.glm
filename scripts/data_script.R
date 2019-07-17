# set.seed(1001)
#
# {
#   k <- 5
#   n <- 5000
#
#   X <- matrix(rnorm(n * k), ncol = 5)
#   p <- rnorm(k)
#
#   rho <- X %*% p
#
#   y <- rpois(n, exp(rho))
#
#   df <- data.frame(cbind(X, y))
#
#   sim.1 <- list("data" = df, "params" = p)
#
#   usethis::use_data(sim.1)
# }
#
# {
#   k <- 5
#   n <- 5000
#
#   X <- matrix(rnorm(n * k), ncol = 5)
#   p1 <- rnorm(k)
#   p2 <- rnorm(k)
#
#   rho <- c(X[1:2500,] %*% p1, X[2501:5000,] %*% p2)
#
#   y <- rpois(n, exp(rho))
#
#   df <- data.frame(cbind(X, y))
#
#   sim.2 <- list("data" = df, "p1" = p1, "p2" = p2)
#
#   usethis::use_data(sim.2)
# }
#
#
# {
#   n <- 10000
#   k <- 10
#   X <-  matrix(
#     rnorm(n * k),
#     ncol = k
#   )
#   exposure <- sample(10:30, n, replace=TRUE)
#
#   p1 <- rnorm(k, 0, 0.5)
#   p2 <- rnorm(k, 0, 0.5)
#
#   ratio <- 0.5
#   cutoff <- floor(ratio * n)
#
#   rho <- c(
#     X[1:cutoff, ] %*% p.1,
#     X[(1 + cutoff):n, ] %*% p.2
#   )
#
#   class.id <- c(
#     rep(1, cutoff),
#     rep(2, n - cutoff)
#   )
#
#   Y <- rpois(n, exposure * exp(rho))
#
#   sim.3 <- list(x= X, y = Y, exposure = exposure, p1 = p1, p2 = p2)
#
#   usethis::use_data(sim.3, overwrite = TRUE)
# }
#
