### Title:    Subroutine runCell
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2022-01-05
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs 
###           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    fs,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[13, ]
  # rp = 1

# Data Generation ---------------------------------------------------------

  # Define target eigen values
  target_eigen <- c(seq(10, 5, length.out = parms$K),
                    sort(runif(parms$P - parms$K,
                               min = 0.01, max = 0.5),
                         decreasing = TRUE))

  # Generate data
  dat_objs <- generatePCdat(N = parms$N,
                            J = parms$P,
                            Q = parms$K,
                            p = 0.4)
  dat_orig <- dat_objs$X

  # Generate a dependent variable
  y <- dat_objs$T %*% rep(1, parms$K)
  R2 <- .90
  SStot <- sum((y - mean(y))^2)
  SSres <- SStot * (1 - R2)
  evar <- SSres / (parms$N-1)
  e <- rnorm(parms$N, 0, sqrt(evar))
  y <- y + e

  # Discretise
  n_var_cate <- parms$P * cond$D # number of categorical variables
  index_disc <- tail(1:parms$P, n_var_cate) # Discrete variables
  index_cont <- which(!(1:parms$P %in% index_disc)) # Continuous variables
  col_disc <- lapply(dat_orig[, index_disc],
                     disData,
                     K = cond$K,
                     interval = cond$interval)

  dat_disc <- cbind(dat_orig[, index_cont, drop = FALSE],
                    col_disc)

  # Disjunction table
  dat_fact <- as.data.frame(lapply(as.data.frame(dat_disc[, index_disc]),
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
  pcs_orig <- extractPCs(dat_orig, y_cv = y, cor_type = "cor")

  # PCA Numerical
  pcs_nume <- extractPCs(dat_disc, y_cv = y, cor_type = "cor")

  # PCA Polychoric
  pcs_poly <- extractPCs(dat_disc, y_cv = y, cor_type = "mixed")

  # PCA Disjunction table
  pcs_disj <- extractPCs(dat_disj, y_cv = y, cor_type = "cor")

  # PCA Dummy
  pcs_dumm <- extractPCs(dat_dumm, y_cv = y, cor_type = "cor")

  # PCAmix
  dat_disc_quanti <- dat_disc[, index_cont]
  dat_disc_quali <- dat_disc[, index_disc]
    dat_disc_quali <- as.data.frame(lapply(dat_disc_quali, factor))
  if(ncol(dat_disc_quanti) == 0){
    pcamix <- PCAmix(X.quali = dat_disc_quali,
                     rename.level = TRUE,
                     ndim = ncol(dat_dumm), graph = FALSE)
  }
  if(ncol(dat_disc_quanti) != 0){
    pcamix <- PCAmix(X.quanti = dat_disc_quanti,
                     X.quali = dat_disc_quali,
                     rename.level = TRUE,
                     ndim = ncol(dat_dumm), graph = FALSE)
  }
  pcamixn_pcs <- pcCV(y = y, X = pcamix$ind$coord, K = 10)
  pcamix_dat <- pcamix$ind$coord[, 1:pcamixn_pcs, drop = FALSE]
  pcamix_r2 <- round(pcamix$eig[pcamixn_pcs, "Cumulative"], 3)/100

  # Append results
  pcs_list <- list(
    orig = pcs_orig,
    nume = pcs_nume,
    poly = pcs_poly,
    disj = pcs_disj,
    dumm = pcs_dumm,
    PCAmix = list(dat = pcamix_dat,
                  r2 = pcamix_r2,
                  npcs = pcamixn_pcs)
  )
  dts_pcs <- lapply(pcs_list, "[[", "dat")

  # Number of PCs extracted
  r2 <- lapply(pcs_list, "[[", "r2")

  # Cor with dv
  cors <- lapply(dts_pcs, function(x){
    abs(colMeans(cor(x = x, y = y)))
  })

  # PCR MSE
  mses <- lapply(dts_pcs, extractMSE, y = y, train = train, test = test)

# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- cbind(cond, r2 = r2, cors = cors, mses = mses)

  ## Return it
  saveRDS(output,
          file = paste0(fs$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}