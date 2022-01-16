# Project:   pcr_discrete
# Objective: extract PCs with the polychoric method
# Author:    Edoardo Costantini
# Created:   2022-01-16
# Modified:  2022-01-16

extractPCsMixed <- function(dt = matrix(), keep = 1L) {
# Internals ---------------------------------------------------------------

  # dt = dat_disc # MASS::mvrnorm(1e2, rep(0, 3), diag(3)) # some dataset
  # keep = .9 # Y variable used for cross-validation to choose CVs

# Body --------------------------------------------------------------------
  # Extract PCs with psych mixed method (polychoric etc.)
  pcr_out <- psych::principal(dt,
                              nfactors = ncol(dt),
                              cor = "mixed",
                              rotate = "none"
  )

  # Store the PC scores
  T <- pcr_out$scores

  # Check the type of keep input (proportion of variane or number?)
  if (is.double(keep)) {
    # Compute a vector of cumulative proportions of explained variances
    CPVE <- cumsum(prop.table(pcr_out$values))

    # Set npcs to the firt PC that explains more than target
    npcs <- Position(function(x) x >= keep, CPVE)

    # Store the actual explained variance by that number of PCs
    r2 <- CPVE[npcs]
  } else {
    # Set npcs to the integer value provided
    npcs <- keep

    # Compute CPVE
    r2 <- cumsum(prop.table(pcr_out$values))[npcs]
  }

  # Store
  return(list(T    = T[, 1:npcs, drop = FALSE],
              npcs = npcs,
              r2   = round(r2, 3)))
}
