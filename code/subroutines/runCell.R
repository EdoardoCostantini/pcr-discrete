### Title:    Subroutine runCell
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-10-19
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs 
###           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    settings,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[13, ]
  # rp = 1

# Data Generation ---------------------------------------------------------
  
  # Generate Continuous Data
  Sigma_blocks <- lapply(1:cond$blocks, function (x){
    Sigma <- matrix(cond$rho,
                    nrow = parms$P/cond$blocks,
                    ncol = parms$P/cond$blocks)
    diag(Sigma) <- 1
    Sigma
  })
  Sigma <- Matrix::bdiag(Sigma_blocks)
  mu <- rep(parms$item_mean, parms$P)
  dat_orig <- data.frame(MASS::mvrnorm(parms$N, mu, Sigma))
    colnames(dat_orig) <- paste0("z", 1:ncol(dat_orig))

  # Discretise
  n_var_cate <- ceiling((parms$P-1) * cond$D) # number of categorical variables
  index_cont <- 1 : (parms$P - n_var_cate) # Continuous variables
  index_disc <- tail(1:parms$P, n_var_cate) # Discrete variables

  dat_disc <- lapply(dat_orig[, -index_cont],
                     disData,
                     K = cond$K,
                     interval = cond$interval)

  dat_disc <- cbind(dat_orig[, index_cont, drop = FALSE], dat_disc)

  # Disjunction table
  dat_fact <- as.data.frame(lapply(as.data.frame(dat_disc[, -index_cont]),
                                   factor, ordered = FALSE))
  dat_disj <- cbind(dat_disc[, index_cont, drop = FALSE],
                    tab.disjonctif(dat_fact))

  # Dummy Coded version
  dat_dumm <- model.matrix(~ .,
                           cbind(dat_orig[, index_cont, drop = FALSE],
                                 dat_fact))[, -1]

  # Define Training and Testing data
  ind   <- sample(1 : nrow(dat_orig))
  train <- ind[1 : (.8*nrow(dat_orig))]
  test  <- ind[(.8*nrow(dat_orig)+1) : nrow(dat_orig)]

# Analysis ----------------------------------------------------------------

  # PCA Original
  pcs_orig <- extractPCs(dat_orig, npcs = cond$blocks, cor_type = "cor")

  # PCA Numerical
  pcs_nume <- extractPCs(dat_disc, npcs = cond$blocks, cor_type = "cor")

  # PCA Polychoric
  pcs_poly <- extractPCs(dat_disc, npcs = cond$blocks, cor_type = "mixed")

  # PCA Disjunction table
  pcs_disj <- extractPCs(dat_disj, npcs = cond$blocks, cor_type = "cor")

  # PCA Dummy
  pcs_dumm <- extractPCs(dat_dumm, npcs = cond$blocks, cor_type = "cor")

  # PCAmix
  dat_disc_quanti <- dat_disc[, index_cont[-1]]
  dat_disc_quali <- as.data.frame(dat_disc[, -index_cont])
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
  pcs_list <- list(
    orig = pcs_orig,
    nume = pcs_nume,
    poly = pcs_poly,
    disj = pcs_disj,
    dumm = pcs_dumm,
    PCAmix = list(dat = pcamix_dat,
                  r2 = pcamix_r2)
  )

  # Number of PCs extracted
  r2 <- lapply(pcs_list, "[[", "r2")

  # PCR MSE
  dts_pcs <- lapply(pcs_list, "[[", "dat")
  mses <- lapply(dts_pcs, extractMSE, train = train, test = test)

# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- cbind(cond, r2 = r2, mses = mses)

  ## Return it
  saveRDS(output,
          file = paste0(settings$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}