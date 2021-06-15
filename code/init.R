### Title:    Defining Fixed Parameters
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-10

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan",
                 "rlecuyer",
                 "parallel",
                 "MLmetrics",
                 "PCAmixdata")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Functions ----------------------------------------------------------

  # Simulation
  source("./fun_runCell.R")
  source("./fun_doRep.R")

  # Support Functions
  source("./helper/fun_dataGen.R")
  source("./helper/functions.R")

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()

  # Generic
  # parms$outDir <- paste0("../output/", format(Sys.time(), "%Y%d%m_%H%M%S"), "/")
  #   dir.create(parms$outDir)
  parms$dt_rep <- 500

  # Seed related
  parms$seed <- 2021
  parms$nStreams <- 1000

  # Data generation
  parms$N <- 1e3 # sample size
  parms$L <- 8 # number of latent variables
  parms$J <- 3 # number of measured items for latent variable
  parms$K <- 5 # number of categories for ordinal variables
  parms$P <- parms$L*parms$J # number of latent variables
  parms$pm <- .2 # proportion of missings level
  parms$fl <- .8 # factor loadings level
  parms$lv_mean   <- 0 # true latent mean
  parms$lv_var    <- 1 # true latent variance
  parms$lv_cov_ta <- .8 # true latent cov for target variables
  parms$lv_cov_mp <- .8 # for mar predictors
  parms$lv_cov_ax <- .8 # for auxiliary set
  parms$item_mean <- 0 # true item mean
  parms$item_var  <- 1 # true item variance
  
  # Map variables
  parms$varMap <- list(ta = 1:2,  # TArget of analysis
                       mp = 3:5, # Mar Predictors
                       ax = 6:parms$L # Auxiliary variables
  )

# Experimental Conditions -------------------------------------------------

  # Parallel Experiments: for the continuous and attenuated relationship
  # Alternative experimental factor
  n_cate <- c(10, 7, 5, 3, 2)

  # Make Conditionsa
  conds <- expand.grid(N  = parms$N,
                       L  = parms$L,
                       J  = parms$J,
                       P  = parms$L * parms$J,
                       fl = parms$fl,
                       K = n_cate,
                       stringsAsFactors = FALSE)

  # Print
  round(conds, 2)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    paste0(colnames(conds), conds[i, ], collapse = "_")}
  )

