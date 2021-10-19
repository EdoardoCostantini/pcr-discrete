# Project:   ordinality
# Objective: Extract Principal Components with different methods
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2021-10-19

extractPCs <- function(dt = matrix(), npcs = 1, cor_type = "cor"){
  # Given a data set A in matrix for, it extracts the first npcs principal
  # components from A (excluding the first column), and retunrs a dataset
  # with the first column of A cobined with the extracted components.
  # It also retunrs the info regarding the proportion of explained variance
  # by the defined number of components
  # when @cor_tupe = "mixed", psych::principal recognizes which variables
  # need pearson, polyserial, polychoric, tetrachoric correlations

  ## Example input
  # dt = MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # npcs = 1
  # cor_type = c("cor", "mixed")[2]

  # PC
  pcr_out <- psych::principal(dt[, -1],
                              nfactors = npcs,
                              cor = cor_type,
  )

  # Combine Original dependent variable w/ PC predictors
  pcs_dat <- cbind(z1 = dt[, 1], pcr_out$scores)
  cor(cbind(z1 = dt[, 1], pcr_out$scores))
  r2 <- cumsum(prop.table(pcr_out$values))[npcs]

  return(list(dat = pcs_dat,
              r2 = r2))
}