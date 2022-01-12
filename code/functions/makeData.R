# Project:   pcr_discrete
# Objective: Generate data based on a given PC structure (version bypassing T)
# Author:    Edoardo Costantini
# Created:   2022-01-05
# Modified:  2022-01-12
# Notes:     Heavily inspired by https://github.com/trbKnl/sparseWeightBasedPCA/blob/master/R/makeData.R

# Divide the columns of matrix by 2norm
divideByNorm <- function(mat){
    A <- 1 / matrix(apply(mat, 2, function(x){sqrt(sum(x^2))}),
                    nrow(mat),
                    ncol(mat),
                    byrow = T)
    return(mat * A)
}

# Orthogonalize two columns of a matrix by using gramm-schmid
# only use intersection of the non-zero coefficients of the two vectors
# to enforce that the zero's do not change
orthogonalize <- function(A, index1, index2){
    int <- intersect(which(A[, index1] != 0), which(A[, index2] != 0))

    u <- A[int, index1]
    v <- A[int, index2]

    newv <- v - as.numeric((t(u) %*% v) / (t(u) %*% u)) * u
    A[int, index2] <- newv

    return(A)
}

# Create an orthonormal matrix
makeP <- function(A){
    counter <- 0
    #while matrix A is not orthonormal and max iterations is not reached
    #do orthogonalize all columns with respect to each other
    #Do this until the columns are orthogonal,
    #sometimes multiple passes are needed
    while(TRUE != all.equal(t(A) %*% A, diag(ncol(A))) &&  counter < 1000 ){
        for(i in 2:ncol(A)){
            for(j in 1:(i-1)){
                A <- orthogonalize(A, j, i)
            }
        }
        A <- divideByNorm(A)
        counter <- counter + 1
    }
    if(counter < 1000){
        return(list(A=A, status=1))
    } else {
        return(list(A=A, status=0))
    }
}

#' makeDat: creates simulated data based on a PCA model
#'
#' A function that generates a dataset based on a PCA model \eqn{X = XWP^T}, with a sparse \eqn{W} and with W = P and \eqn{W^T W = I}
#'
#' @param n The number of objects the data should have
#' @param ncomp The number of components that are of interest
#' @param comdis A \code{matrix} specifying the zero structure of \eqn{W}, the data will have ncomp = \code{ncol(comdis)} "important components" and J = \code{nrow(comdis)} variables
#' @param variances specifying the variances of the J components these are the J eigenvalues of \eqn{X^T X}
#' @return A list with the following items: \cr
#' \code{X} A data matrix generated from MASS::mvrnorm() with a zero mean structure and Sigma = P \%*\% diag(variances) \%*\% t(P), empirical is set \code{FALSE} \cr
#' \code{P} A matrix of dimension J x J, with the loadings/weights the first Q columns have the sparsity structure specified in \code{comdis}, the other Q-J columns are non-sparse. \cr
#' \code{Sigma} The covariance matrix that is used to generate the data from Sigma \code{= P \%*\% diag(variances) \%*\% t(P)} \cr
#' In case of failure the function returns \code{NA}. The function can fail if the \code{comdis} structure specified in P is not possible, i.e. linear dependency
#'
#' @export
#' @examples
#'
#' set.seed(20220105)
#' K <- 3 # number of components
#' J <- 9 # number of indicators
#' vars <- c(seq(10, 5, length.out = K), sort(runif(J-K, min = 0.01, max = 0.5), decreasing = TRUE))
#'
#' dat   <- makeDat(n = 1e4, variances = vars)
#'

makeDat <- function(n, variances){
  # Generate a random loading matrix P
  J <- length(variances)
  Q <- sum(variances >= 1)
  P <- matrix(data = rnorm(J * Q),
              nrow = J,
              ncol = Q)

  # Orthogonalize P (so that P'P = I)
  P <- makeP(P)$A

  # Generate X
  Sigma <- P %*% diag(variances[1:Q]) %*% t(P)
  X     <- MASS::mvrnorm(n     = n,
                         mu    = rep(0, J),
                         Sigma = Sigma)

  return(list(X = data.frame(X),
              P = P,
              Sigma = Sigma))
}
