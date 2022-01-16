# Title:    Defining Fixed Parameters
# Project:  Ordinality
# Author:   Edoardo Costantini
# Created:  2021-06-10
# Modified: 2022-01-16

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan",
                 "rlecuyer",
                 "parallel",
                 "MLmetrics",
                 "PCAmixdata",
                 "psych",
                 "stringr",
                 "dplyr",
                 "sn",          # for Multivariate Skewed Distribution
                 "fungible",    # for Multivariate Skewed Distribution
                 "testthat",
                 "moments",     # for skewness
                 "FactoMineR")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Functions ----------------------------------------------------------

  # Subroutines
  all_subs <- paste0("./subroutines/",
                     list.files("./subroutines/"))
  lapply(all_subs, source)

  # Functions
  all_funs <- paste0("./functions/",
                     list.files("./functions/"))
  lapply(all_funs, source)

  # Helper
  all_help <- paste0("./helper/",
                     list.files("./helper/"))
  lapply(all_help, source)

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()

  # Generic
  parms$dt_rep <- 1e3

  # Seed related
  parms$seed <- 2021
  parms$nStreams <- 1000

  # Data generation
  parms$N <- 1e3 # sample size
  parms$K <- 3   # true number of principal components
  parms$P <- 10  # number of variables
  parms$item_mean <- 0 # true item mean
  parms$item_var  <- 1 # true item variance

# Experimental Conditions -------------------------------------------------

  # Parallel Experiments: for the continuous and attenuated relationship
  # Alternative experimental factor
  n_cate <- c(7, 5, 3, 2)
  p_cate <- round(seq(.2, 1, length.out = 5), 2)
  npcs   <- as.integer(1:parms$P)
  interval <- c(TRUE)

  # Make Conditionsa
  conds <- expand.grid(N  = parms$N, # sample size
                       P  = parms$P, # number of total variables
                       K = n_cate, # number of categories
                       D = p_cate, # ordinality degree
                       npcs = npcs,
                       interval = interval,
                       stringsAsFactors = FALSE)

  # correct order
  conds <- conds[, c("N", "P", "K", "D", "npcs", "interval")]

  # Print
  round(conds, 2)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    conds$D <- conds$D*100 # to improve tag name
    paste0(colnames(conds), conds[i, ], collapse = "_")
  }
  )

