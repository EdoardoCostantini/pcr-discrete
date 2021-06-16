### Title:    Subroutine runCell
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-16
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs 
###           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    settings,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[4, ]
  # rp = 1

# Data Generation ---------------------------------------------------------
  
  # Generate Continuous Data
  dat_list <- genData(parms = parms, cond = cond)
  dat_cont <- dat_list$dat_ob

  # Discretise
  dat_disc <- apply(dat_cont[,-1], 2, function(j){
    as.numeric(cut(j, breaks = cond$K))
  })

  # dat_disc <- scale(cbind(z1 = dat_cont[,1], dat_disc)) # scale it
  # dat_disc <- cbind(z1 = dat_cont[,1], scale(dat_disc)) # scale it
  dat_disc <- cbind(z1 = dat_cont[, 1], dat_disc) # scale it

  # Generate Continuous Data w/ attenuated relationships
  Sigma <- cor(dat_disc)
  mu <- colMeans(dat_disc)
  dat_disc_cont <- MASS::mvrnorm(parms$N, mu, Sigma)

  # Define Training and Testing data
  ind   <- sample(1 : nrow(dat_cont))
  train <- ind[1 : (.8*nrow(dat_cont))]
  test  <- ind[(.8*nrow(dat_cont)+1) : nrow(dat_cont)]

  # Store datasets in a list
  dts <- list(cont = dat_cont,
              disc = dat_disc,
              atte = dat_disc_cont)
  dts <- lapply(dts, scale)

# Analysis ----------------------------------------------------------------

  # Store Correlations
  cors <- sapply(dts, extract_avg_cor)

  # Estiamtes and CI regression parameters
  coefs <- sapply(dts, extract_lm)

  # PCA results
  pcs_list <- lapply(dts, extract_pcs, npcs = 5)

  # Append results from ad-hoc PCAmix
  cor_type <- ifelse(length(unique(dat_disc[, 2])) >= 8,
                     "cor",
                     "poly")
  principal_out <- principal(dat_disc[, -1], nfactors = 5,
                             cor = cor_type,
                             rotate = "none") #principal components
  p <- print(principal_out)
    principal_r2 <- round(p$Vaccounted, 2)["Cumulative Var", 5]

  # Add y
  PCAmix_dat <- cbind(z1 = dat_cont[, 1], principal_out$scores)

  # Append results
  pcs_list <-   append(pcs_list,
                       list(PCAmix = list(dat = PCAmix_dat,
                                          r2 = principal_r2)
                       )
  )

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

