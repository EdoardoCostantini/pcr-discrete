# Project:   ordinality
# Objective: Function to get the cross-validation error (CVE)
# Author:    Edoardo Costantini
# Created:   2021-11-13
# Modified:  2021-11-13

# Extract DV name from model fit
dvName <- function(x) all.vars(x$terms)[1]

## Compute the cross-validation error:
getCve <- function(model, data, K, part) {
#' @title Compute cross-validation error
#'
#' @description For a given model, it returns its CVE.
#' Given a dv, a set of predictor and a number of folds, this function obtains
#' the cross-validation error (CVE) for the linear models.
#'
#' The function returns a vector of length 1 with the cve.
#'
#' @param model a formula
#' @param data a matrix or data.frame with named columns containing both the
#' dv and the predictors.
#' @param K the number of folds
#' @param part a partitioning vector
#'
#' @examples
#' Z <- MASS::mvrnorm(n = 1e3, rep(0, 10), diag(10))
#' y <- Z %*% rep(1, 10) + rnorm(1e3)
#' data <- data.frame(y = y, Z = Z)
#' model <- y ~ .
#' K <- 10
#' part <- sample(rep(1 : K, ceiling(nrow(data) / K)))[1 : nrow(data)]
#' getCve(model, data, K, part)

    ## Loop over K repititions:
    mse <- rep(NA, K)
    for(k in 1 : K) {
        ## Partition data:
        train <- data[part != k, ]
        valid <- data[part == k, ]

        ## Fit model, generate predictions, and save the MSE:
        fit    <- lm(model, data = train)
        pred   <- predict(fit, newdata = valid)
        mse[k] <- MSE(y_pred = pred, y_true = valid[ , dvName(fit)])
    }
    ## Return the CVE:
    sum((table(part) / length(part)) * mse)
}
