# Project:   ordinality
# Objective: regresses the first column of a matrix on the remaining ones
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2021-10-19

#  -------------------------------------------------

extract_lm <- function(dt = matrix()){
  # Given a data set in matrix form, this function fits a lm and extracts
  # regression coefficient estiamtes
  # Example input
  # dt = MASS::mvrnorm(1e2, rep(0, 3), diag(3))

  # Fit lm model
  lm_fit <- lm(dt[, 1] ~ dt[, 2:3])

  # Extract coefficients
  lm_coefs <- coef(lm_fit)

  # Output
  return(lm_coefs)
}
