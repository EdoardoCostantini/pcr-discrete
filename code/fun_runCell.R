### Title:    Subroutine runCell
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-10
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs 
###           imputations for every condition in the set.

runCell <- function(cond, parms, 
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[5, ]
  # rp = 1
  # cluster = FALSE

# Data Generation ---------------------------------------------------------
  
  # Generate Continuous Data
  dat_list <- genData(parms = parms, cond = cond)
  dat_cont <- dat_list$dat_ob

  # Discretise
  dat_disc <- apply(dat_cont[,-1], 2, function(j){
    as.numeric(cut(j, breaks = cond$K))
  })

  dat_disc <- scale(cbind(z1 = dat_cont[,1], dat_disc)) # scale it

  # Study discretized data
  Sigma <- cor(dat_disc)
  mu <- colMeans(dat_disc)
  dat_disc_cont <- MASS::mvrnorm(parms$N, mu, Sigma)

# Analysis ----------------------------------------------------------------

  # Store Correlations
  cors <- extract_avg_cor(list(cont = cor(dat_cont),
                               disc = cor(dat_disc),
                               atte = cor(dat_disc_cont)))

  # Estiamtes and CI regression parameters
  coefs <- extract_lm(list(cont = dat_cont,
                           disc = dat_disc,
                           atte = dat_disc_cont))

# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- list(cond  = cond,
                 coefs = coefs,
                 cors = cors)

  ## Return it
  saveRDS(output,
          file = paste0(parms$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}

