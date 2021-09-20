### Title:    Subroutine runCell
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-09-20
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs 
###           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    settings,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[3, ]
  # rp = 1

# Data Generation ---------------------------------------------------------
  
  # Generate Continuous Data
  Sigma <- matrix(parms$item_cor,
                  nrow = parms$P, ncol = parms$P)
    diag(Sigma) <- 1
  mu <- rep(parms$item_mean, parms$P)
  dat_orig <- MASS::mvrnorm(parms$N, mu, Sigma)
    colnames(dat_orig) <- paste0("z", 1:ncol(dat_orig))

  # Discretise
  n_var_cate <- ceiling((parms$P-1) * cond$D) # number of categorical variables
  keep_continuous <- 1:(ncol(dat_orig)-n_var_cate)
  dat_disc <- apply(dat_orig[, -keep_continuous],
                    2,
                    dis_data,
                    K = cond$K,
                    interval = cond$interval)
  dat_disc <- cbind(dat_orig[, keep_continuous, drop = FALSE], dat_disc)

  # Disjunction table
  dat_fact <- as.data.frame(lapply(as.data.frame(dat_disc[, -keep_continuous]),
                                   factor, ordered = FALSE))
  dat_disj <- cbind(dat_disc[, keep_continuous, drop = FALSE],
                    tab.disjonctif(dat_fact))

  # Dummy Coded version
  dat_dumm <- model.matrix(~ .,
                           cbind(dat_orig[, keep_continuous, drop = FALSE],
                                 dat_fact))[, -1]

  # Generate Continuous Data w/ attenuated relationships
  Sigma_atte <- cor(dat_disc)
  mu_atte <- colMeans(dat_disc)
  dat_atte <- MASS::mvrnorm(parms$N, mu_atte, Sigma_atte)

  # Define Training and Testing data
  ind   <- sample(1 : nrow(dat_orig))
  train <- ind[1 : (.8*nrow(dat_orig))]
  test  <- ind[(.8*nrow(dat_orig)+1) : nrow(dat_orig)]

  # Store datasets in a list
  dts <- list(orig = dat_orig,
              disc = dat_disc,
              atte = dat_atte,
              disj = dat_disj,
              dumm = dat_dumm)

# Analysis ----------------------------------------------------------------

  # Store Correlations
  cors <- lapply(dts, extract_avg_cor)

  # PCA results
  pcs_list <- lapply(dts, extract_pcs, npcs = parms$npcs)

  # PCA w/ polychoric tetrachoric correlations
  # Get PCs
  pca_poly <- extract_pcs_poly(dat_disc, npcs = parms$npcs)
  pcs_list <-   append(pcs_list, list(poly = pca_poly))

  # PCAmix results
  dat_disc_quanti <- dat_disc[, keep_continuous[-1]]
  dat_disc_quali <- as.data.frame(dat_disc[, -keep_continuous])
    dat_disc_quali <- as.data.frame(lapply(dat_disc_quali, factor))
  if(ncol(dat_disc_quanti) == 0){
    pcamix <- PCAmix(X.quali = dat_disc_quali,
                     rename.level = TRUE,
                     ndim = ncol(dat_disc), graph = FALSE)
  }
  if(ncol(dat_disc_quanti) != 0){
    pcamix <- PCAmix(X.quanti = dat_disc_quanti,
                     X.quali = dat_disc_quali,
                     rename.level = TRUE,
                     ndim = ncol(dat_disc), graph = FALSE)
  }
  pcamix_dat <- cbind(z1 = dat_disc[, 1],
                      PC = pcamix$ind$coord[, 1:parms$npcs, drop = FALSE])
  var_exp <- apply(pcamix$ind$coord, 2, var) # same
  var_tot <- sum(var_exp)
  pcamix_r2 <- unname( # cumulative variance explained
    round(cumsum(var_exp)/var_tot, 3)[1:parms$npcs]
  )

  # Append results
  pcs_list <-   append(pcs_list,
                       list(PCAmix = list(dat = pcamix_dat,
                                          r2 = pcamix_r2)
                       )
  )

  # Number of PCs extracted
  r2 <- lapply(pcs_list, "[[", "r2")

  # PCR MSE
  dts_pcs <- lapply(pcs_list, "[[", "dat")
  mses <- lapply(dts_pcs, extract_mse, train = train, test = test)

# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- cbind(cond, cors = cors, r2 = r2, mses = mses)

  ## Return it
  saveRDS(output,
          file = paste0(settings$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}