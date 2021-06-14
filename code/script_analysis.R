### Title:    Analysing results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-14

  ## Make sure we have a clean environment:
  rm(list = ls(all = TRUE))

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  runName <- "20211406_151337"

  # Extract last element as session info object
  res <- readRDS(paste0(inDir, runName, "_res.rds"))

  # Support Functions
  source("./helper/functions.R")

# Analyse -----------------------------------------------------------------

  # Possible Results
  names(res$results$N1000_L8_J3_P24_fl0.8_K2$rep1)

  # Mean result by name
  lapply(res$results, average_result, result = "coefs")
  lapply(res$results, average_result, result = "cors")
  lapply(res$results, average_result, result = "r2")
  lapply(res$results, average_result, result = "mses")