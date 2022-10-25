# Project:   pcr_discrete
# Objective: Generate data based on a given PC structure (version giving T)
# Author:    Edoardo Costantini
# Created:   2022-01-05
# Modified:  2022-10-25
# Source:    https://github.com/trbKnl/SCaDS/blob/master/Simulation_study_4.1/generateData.R
#            https://github.com/soogs/SCD-CovR/blob/master/functions/spcovrdata.R

generateXTP <- function(
  I, 
  J, 
  VAFr = c(.5, .4, .2), 
  VAFsum = 100,
  CPVE = 0.9,
  skewness = rep(0, length(VAFr)), 
  kurtosis = rep(0, length(VAFr))
){

# Internals -------------------------------------------------------------

  # I    = 1e5 # sample size
  # J    = 9 # number of variables
  # VAFr = c(.5, .3, .2) # relative variance of each components
  # VAFsum = 100 # total variance of the components
  # CPVE = 0.9 # proportion of explained variance by the R components
  # skewness = rep(0, length(VAFr))
  # kurtosis = rep(0, length(VAFr))

# Body ------------------------------------------------------------------
  # Number of components
  R <- length(VAFr)

  # Random sample U with skewness defined by parameters
  U <- sapply(1:R, function(j) {
    # Collect skewness and kurtosis in a data.frame
    cp_sk <- cbind(s = skewness, k = kurtosis)

    # Define direct parameterization for the skew-t (ST) distribution
    cpST <- c(0, 1, cp_sk[j, "s"], cp_sk[j, "k"])
    dpST <- cp2dp(cpST, family = "ST")

    # Sample from skew-t distribution
    rst(n = I, dp = dpST)
  })

  # Scale and orthonormalize
  U <- scale(U, center = TRUE, scale = FALSE)
  U <- orthmat(U, verbose = FALSE)
  U <- normmat(U)

  # Random sample P
  V <- matrix(
    data = runif(J * R),
    nrow = J,
    ncol = R
  )
  V <- orthmat(V, verbose = FALSE)
  P <- normmat(V)

  # Define D
  D <- diag(c(VAFsum * VAFr))

  # Create X
  Xtrue  <- U %*% D %*% t(P)

  # sample from normal distribution (Ex = Error of X)
  Ex <- MASS::mvrnorm(n = I, mu = rep(0, J), Sigma = diag(J))

  # centering and scaling the Ex matrix
  Ex <- scale(Ex, center = TRUE, scale = FALSE)

  # sum of squares
  ssqXtrue <- sum(Xtrue^2)
  ssqEx <- sum(Ex^2)

  # Compute noise scaling constant
  Escale <- sqrt(ssqXtrue * (1 - CPVE) / (CPVE * ssqEx))

  # Add scaled noise
  X  <- Xtrue + Escale * Ex

  # Scale data for estimation
  X <- scale(X)

  # Store skewness of data
  Sk <- apply(X, 2, e1071::skewness, type = 3)

  # Define outputs
  return(list(X = data.frame(X),
              T = U %*% D,
              P = P,
              U = U,
              D = D,
              skew = Sk)
  )
}
