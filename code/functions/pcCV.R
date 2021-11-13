# Project:   ordinality
# Objective: Function to cross-validate the number of components
# Author:    Edoardo Costantini
# Created:   2021-11-12
# Modified:  2021-11-13

pcCV <- function(y, X, K = 10){
#' @title Principal Components cross-validation
#'
#' @description Performs the cross-validation to select the number of componets
#' to retain
#'
#' Given a dv, a set of componets extrscted from an original set of predictors,
#' and a number of forlds, this function obtains the cross-validation error (CVE)
#' for the linear models predicting the dv with an increasing number of comp
#' oents.
#'
#' The function returns a vector of length 1 with the number of components that
#' returns the lowest CVE.
#'
#' @param y: a vector containing a dependent variable used to compute the cve
#' @param X: a matrix containing the scores on all the PCs obtained on some
#'      set of predictors.
#' @examples
#' Z <- MASS::mvrnorm(n = 1e3, rep(0, 10), diag(10))
#' y <- Z %*% rep(1, 10) + rnorm(1e3)
#' X <- prcomp(Z)$x

  # Define useful objects
  tot_npcs <- ncol(X)

  # Crossvalidate to chose npcs
  data <- (data.frame(y = y, X = X))

  ## Create a partition vector
  part <- sample(rep(1 : K, ceiling(nrow(data) / K)))[1 : nrow(data)]

  ## Apply over models using more and more components:
  cve <- sapply(1:tot_npcs, function(i){
    getCve(model = y ~ ., data = data[, 1:(i+1)], K = K, part = part)
  })
  npcs <- which.min(cve)

  return(npcs)

}
