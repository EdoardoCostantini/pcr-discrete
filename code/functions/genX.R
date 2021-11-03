# Project:   ordinality
# Objective: data generation function
# Author:    Edoardo Costantini
# Created:   2021-11-02
# Modified:  2021-11-02

genX <- function (parms, cond){

  # Generate Continuous Data
  Sigma_blocks <- lapply(1:cond$blocks, function (x){
    Sigma <- matrix(cond$rho,
                    nrow = parms$P/cond$blocks,
                    ncol = parms$P/cond$blocks)
    diag(Sigma) <- 1
    Sigma
  })
  Sigma <- as.matrix(Matrix::bdiag(Sigma_blocks))
  mu <- rep(parms$item_mean, parms$P)
  monte_object <- monte1(seed = NULL,
                         nsub = parms$N,
                         nvar = parms$P,
                         skewvec = rep(cond$skewness, parms$P),
                         kurtvec = rep(0, parms$P),
                         cormat = Sigma)
  x <- data.frame(monte_object$data)

  return(x)
}
