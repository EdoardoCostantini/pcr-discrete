# Project:   pcr_discrete
# Objective: extract PCs with the PCAmix method
# Author:    Edoardo Costantini
# Created:   2022-01-16
# Modified:  2022-01-16

extractPCAmix <- function(dt = matrix(), keep = 1L, index_cont, index_disc) {
# Internals ---------------------------------------------------------------

  # dt = dat_disc # MASS::mvrnorm(1e2, rep(0, 10), diag(10))
  # keep = 1 # either and integer specifying the number of components or a
  #           # double specifying the proportion of variance explained that
  #           # should be kept
  # index_cont = c(1:2)
  # index_disc = c(3:10)

# Body --------------------------------------------------------------------
  # Define indexing objects for variable types
  dt_quanti <- dt[, index_cont]
  dt_quali <- as.data.frame(
    lapply(dt[, index_disc], factor)
  )

  # Extract components
  if(ncol(dt_quanti) == 0){
    pcamix <- PCAmix(X.quali = dt_quali,
                     rename.level = TRUE,
                     ndim = ncol(dat_dumm), graph = FALSE)
  }
  if(ncol(dt_quanti) != 0){
    pcamix <- PCAmix(X.quanti = dt_quanti,
                     X.quali = dt_quali,
                     rename.level = TRUE,
                     ndim = ncol(dt), graph = FALSE)
  }

  # Store the PC scores
  T <- pcamix$ind$coord

  # Check the type of keep input (proportion of variane or number?)
  if (is.double(keep)) {
    # Compute a vector of cumulative proportions of explained variances
    CPVE <- pcamix$eig[, "Cumulative"]/100

    # Set npcs to the firt PC that explains more than target
    npcs <- Position(function(x) x >= keep, CPVE)

    # Store the actual explained variance by that number of PCs
    r2 <- CPVE[npcs]
  } else {
    # Set npcs to the integer value provided
    npcs <- keep

    # Compute CPVE
    r2 <- pcamix$eig[1:npcs, "Cumulative"]/100
  }

  # Store
  return(list(T    = T[, 1:npcs, drop = FALSE],
              npcs = npcs,
              r2   = round(r2, 3)))

}
