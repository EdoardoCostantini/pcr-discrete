# Project:   ordinality
# Objective: Extract Principal Components with different methods
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2021-11-13

extractPCs <- function(dt = matrix(), y_cv, cor_type = "cor"){
  # Given a data set A in matrix for, it extracts the first npcs principal
  # components from A (excluding the first column), and retunrs a dataset
  # with the first column of A cobined with the extracted components.
  # It also retunrs the info regarding the proportion of explained variance
  # by the defined number of components
  # when @cor_tupe = "mixed", psych::principal recognizes which variables
  # need pearson, polyserial, polychoric, tetrachoric correlations

  ## Example input
  # dt = dat_orig # MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # y_cv = y # Y variable used for cross-validation to choose CVs
  # cor_type = c("cor", "mixed")[2]

  # PC
  pcr_out <- psych::principal(dt,
                              nfactors = ncol(dt),
                              cor = cor_type,
                              rotate = "none"
  )

  # Crossvalidate to chose npcs
  npcs <- pcCV(y = y_cv, X = pcr_out$scores, K = 10)

  # Compute CPVE
  r2 <- cumsum(prop.table(pcr_out$values))[npcs]

  # Store
  return(list(dat = pcr_out$scores[, 1:npcs],
              r2 = r2,
              npcs = npcs))
}
