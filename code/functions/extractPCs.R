# Project:   ordinality
# Objective: Extract Principal Components with different methods
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2022-01-16

extractPCs <- function(dt = matrix(), keep = 1L){
# Description -------------------------------------------------------------

  # Given a data set A in matrix for, it extracts the first keep principal
  # components from A, and returns a dataset
  # with the first column of A cobined with the extracted components.
  # It also retunrs the info regarding the proportion of explained variance
  # by the defined number of components
  # when @cor_tupe = "mixed", psych::principal recognizes which variables
  # need pearson, polyserial, polychoric, tetrachoric correlations

# Internals ---------------------------------------------------------------

  # dt = dat_disc # MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # keep = 3L # either and integer specifying the number of components or a
  #           # double specifying the proportion of variance explained that
  #           # should be kept

# Body --------------------------------------------------------------------
  # Make sure data is scaled
  dt <- scale(dt)

  # SVD decomposition
  svd_out <- svd(dt)

  # Compute the PC scores
  T <- (svd_out$u %*% diag(svd_out$d))

  # Check the type of keep input (proportion of variane or number?)
  if (is.double(keep)) {
    # Compute a vector of cumulative proportions of explained variances
    CPVE <- cumsum(prop.table(svd_out$d^2))

    # Set npcs to the firt PC that explains more than target
    npcs <- Position(function(x) x >= keep, CPVE)

    # Store the actual explained variance by that number of PCs
    r2 <- CPVE[npcs]
  } else {
    # Set npcs to the integer value provided
    npcs <- keep

    # Compute CPVE
    r2 <- cumsum(prop.table(svd_out$d^2))[npcs]
  }

  # Store
  return(list(T    = T[, 1:npcs, drop = FALSE],
              npcs = npcs,
              r2   = round(r2, 3)))
}
