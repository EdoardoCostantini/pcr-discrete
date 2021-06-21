### Title:    Subroutine runCell
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-21
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs 
###           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    settings,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[20, ]
  # rp = 1

# Data Generation ---------------------------------------------------------
  
  # Generate Continuous Data
  Sigma <- matrix(parms$item_cor,
                  nrow = parms$P, ncol = parms$P)
    diag(Sigma) <- 1
  mu <- rep(parms$item_mean, parms$P)
  dat_cont <- MASS::mvrnorm(parms$N, mu, Sigma)
    colnames(dat_cont) <- paste0("z", 1:ncol(dat_cont))

  # Discretise
  n_var_cate <- (parms$P-1) * cond$D # number of categorical variables
  keep_continuous <- 1:(n_var_cate+1)
  dat_disc <- apply(dat_cont[, -keep_continuous],
                    2, function(j){
      as.numeric(cut(j, breaks = cond$K))
    })

  dat_disc <- cbind(dat_cont[, keep_continuous], dat_disc)

  # Generate Continuous Data w/ attenuated relationships
  Sigma_atte <- cor(dat_disc)
  mu_atte <- colMeans(dat_disc)
  dat_atte <- MASS::mvrnorm(parms$N, mu_atte, Sigma_atte)

  # Define Training and Testing data
  ind   <- sample(1 : nrow(dat_cont))
  train <- ind[1 : (.8*nrow(dat_cont))]
  test  <- ind[(.8*nrow(dat_cont)+1) : nrow(dat_cont)]

  # Store datasets in a list
  dts <- list(cont = dat_cont,
              disc = dat_disc,
              atte = dat_atte)

# Analysis ----------------------------------------------------------------

  # Store Correlations
  cors <- sapply(dts, extract_avg_cor)

  # Estimates and CI regression parameters
  coefs <- sapply(dts, extract_lm)

  # PCA results
  pcs_list <- lapply(dts, extract_pcs, npcs = parms$npcs)

  # Number of PCs extracted
  r2 <- sapply(pcs_list, "[[", "r2")

  # PCR MSE
  dts_pcs <- lapply(pcs_list, "[[", "dat")
  mses <- sapply(dts_pcs, extract_mse, train = train, test = test)

# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- list(cond  = cond,
                 coefs = coefs,
                 cors = cors,
                 r2 = r2,
                 mses = mses)

  ## Return it
  saveRDS(output,
          file = paste0(settings$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}

