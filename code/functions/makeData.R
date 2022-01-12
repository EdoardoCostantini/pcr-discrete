# Project:   pcr_discrete
# Objective: Generate data based on a given PC structure (version bypassing T)
# Author:    Edoardo Costantini
# Created:   2022-01-05
# Modified:  2022-01-12
# Notes:     Heavily inspired by https://github.com/trbKnl/sparseWeightBasedPCA/blob/master/R/makeData.R

makeDat <- function(n, vs){
    ## Inputs
    # n = 10    # sample size
    # vs = makeVariance(c(100, 80, 70), J = J, error = 0.05)

    ## Body
    # Generate a random loading matrix P
    J <- length(vs)
    P <- matrix(data = rnorm(J * J),
                nrow = J,
                ncol = J)

    # Orthogonalize P (so that P'P = I)
    P <- orthmat(P, verbose = FALSE)

    # Generate X
    Sigma <- P %*% diag(vs) %*% t(P)
    X     <- MASS::mvrnorm(n     = n,
                           mu    = rep(0, J),
                           Sigma = Sigma)

    # Output
    return(list(X = data.frame(X),
                P = P,
                Sigma = Sigma))
}

# Function to orthogonalise matrices
orthmat <- function(X, verbose = TRUE) {
    for (i in 2:ncol(X)) {
        for (j in 1:(i - 1)) {
            if (verbose == TRUE) {
                print(paste0("Adjusting piar ", i, "-", j))
            }
            A <- X[, j]
            b <- X[, i]

            # Step 1: find projection of b on A
            B <- as.vector(b - (t(A) %*% b / t(A) %*% A) %*% A)

            # Replace in original X the orthogonalized columns
            X[, j] <- A
            X[, i] <- B
        }
    }

    # Step 2: Divide both by l2-norm (length)
    X <- apply(X, 2, function(j) j / sqrt(sum(j^2)))

    return(X)
}

makeVariance <- function(varianceOfComps, J, error){
    #function to create variances for the components
    #you have to supply the variances of the components you are interested in
    #the variances of the other non interesting components are on a log scale
    #starting with mean(variances of interesting components) /2
    #these variances then get scaled such that error ratio is gotten.
    ncomp <- length(varianceOfComps)
    varsOfUnimportantComps <- exp(seq(log(0.0001),
                                      log(min(varianceOfComps)/2),
                                      length.out = J - ncomp))[(J - ncomp):1]

    x <- (-error*sum(varianceOfComps) / (error-1)) / sum(varsOfUnimportantComps)

    return(c(varianceOfComps, x * varsOfUnimportantComps))
}