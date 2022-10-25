# Project:  pcr_discrete
# Author:   Edoardo Costantini
# Created:  2021-06-10
# Modified: 2022-01-25
# Note:     A "cell" is a cycle through the set of conditions.
#           The function in this script generates 1 data set, performs
#           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    fs,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[288, ]
  # rp = 1

# Data Generation ---------------------------------------------------------

  # Generate data
  XTP <- generateXTP(
    I = parms$N,
    J = parms$P,
    VAFr = parms$XTP_VAFr,
    VAFsum = parms$XTP_VAFsum,
    CPVE = parms$XTP_R2,
    skewness = as.numeric(cond[, grep("skewness", names(cond))]),
    kurtosis = as.numeric(cond[, grep("kurtosis", names(cond))])
  )
  dat_orig <- XTP$X

  # Generate a dependent variable on true line
  y <- generateDV(X = XTP$T,
                  R2 = parms$yT_R2,
                  beta = parms$yT_beta)

  # Define the number of categorical variables for this condition
  n_var_cate <- parms$P * cond$D

  # Define an index for the discrete variables
  index_disc <- tail(1:parms$P, n_var_cate)

  # Define an index for the continuous variables
  index_cont <- which(!(1:parms$P %in% index_disc))

  # Discretize variables based on these indexes
  disData_out <- lapply(dat_orig[, index_disc],
                        disData,
                        K = cond$K,
                        interval = cond$interval,
                        min_bin = parms$min_bin)

  # Extract discretized columns
  col_disc <- lapply(disData_out, "[[", "x")

  # Combine the discretized variables with the ones that stayed continuous
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
  pcs_orig <- extractPCs(dt = dat_orig,
                         keep = as.character(cond$npcs))

  # PCA Numerical
  pcs_nume <- extractPCs(dt = dat_disc,
                         keep = as.character(cond$npcs))

  # PCA Disjunction table
  pcs_disj <- extractPCs(dt = dat_disj,
                         keep = as.character(cond$npcs))

  # PCA Dummy
  pcs_dumm <- extractPCs(dt = dat_dumm,
                         keep = as.character(cond$npcs))

  # PCA Polychoric
  pcs_poly <- extractPCsMixed(dt = dat_disc,
                              keep = as.character(cond$npcs))

  # PCAmix
  pcs_PCAmix <- extractPCAmix(dt = dat_disc,
                              keep = as.character(cond$npcs),
                              index_cont = index_cont,
                              index_disc = index_disc)

  # TODO: think of adding Robust PCA at least with MCD approach
  #        

  # Append results
  pcs_list <- list(
    orig = pcs_orig,
    nume = pcs_nume,
    poly = pcs_poly,
    disj = pcs_disj,
    dumm = pcs_dumm,
    PCAmix = pcs_PCAmix
  )

  # Extract datasets of PC predictors
  dts_pcs <- lapply(pcs_list, "[[", "T")

  # Extract number of PCs extracted
  npcs <- lapply(pcs_list, "[[", "npcs")

  # Extract CPVE by the npcs
  r2 <- lapply(pcs_list, "[[", "r2")

  # Cor with dv
  cors <- lapply(dts_pcs, function(x){
    abs(colMeans(cor(x = x, y = y)))
  })

  # PCR MSE
  mses <- lapply(dts_pcs, extractMSE, y = y, train = train, test = test)

# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- cbind(cond, npcs = npcs, r2 = r2, cors = cors, mses = mses)

  ## Return it
  saveRDS(output,
          file = paste0(fs$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}