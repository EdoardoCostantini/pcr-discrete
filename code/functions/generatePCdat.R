# Project:   pcr_discrete
# Objective: Generate data based on a given PC structure (version giving T)
# Author:    Edoardo Costantini
# Created:   2022-01-05
# Modified:  2022-01-05
# Source:    https://github.com/trbKnl/SCaDS/blob/master/Simulation_study_4.1/generateData.R

#' generatePCdat: creates simulated data based on a PCA model
#'
#' A function that generates a dataset based on a PCA model X = XWP'
#'
#' @param N Sample size
#' @param J Number of variables in the final data set
#' @param Q Number of components of interest
#' @param p proportion of noise
#' @return A list
#' @example
# out <- generatePCdat(J = 10,
#                      N = 1e3,
#                      Q = 1,
#                      p = 0.4)
#
# egn <- eigen(cov(out$X))$values
# round(egn, 3)
#
# prop.table(egn)

generatePCdat <- function(N, J, Q, p){

  # Generating T = XW
  X     <- matrix(data = rnorm(N * J, 0, 1),
                  nrow = N,
                  ncol = J)
  V     <- svd(X)$v
  W     <- V[, 1:Q]
  T     <- X %*% W

  # Generating P
  XTX   <- t(X) %*% X
  SVD   <- svd(XTX %*% W)
  P     <- SVD$u %*% t(SVD$v)

  # Generating X = TP'
  X     <- T %*% t(P)
  Xtrue <- X

  # Adding error X = TP' + E
  E <- matrix(rnorm(J*N, 0, 1), N, J)
  g <- sqrt((var(as.vector(Xtrue)) * p) / (var(as.vector(E)) * (1 - p)))
  X <- Xtrue + E * g
  SStrue <- var(as.vector(Xtrue))
  SSX <- var(as.vector(X))

  return(list(X = data.frame(X),
              T = T,
              W = W,
              P = P,
              errorRatio = 1 - SStrue/SSX)
  )
}
