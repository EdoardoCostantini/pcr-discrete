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
  cpM <- list(mean = mu,
              var.cov = Sigma,
              gamma1 = rep(cond$skewness, parms$P))
  dpM <- cp2dp(cpM, family = "SN")
  x <- rmsn(parms$N, dp = dpM)
    colnames(x) <- paste0("z", 1:ncol(x))

  return(x)
}
