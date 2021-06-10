### Title:    Analysing results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-10

  ## Make sure we have a clean environment:
  rm(list = ls(all = TRUE))

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  runName <- "20211006_175207"

  # Extract last element as session info object
  res <- readRDS(paste0(inDir, runName, "_res.rds"))

  # Support Functions
  source("./helper/functions.R")

# Analyse -----------------------------------------------------------------

  # Mean linear regression estimates
  lapply(res$results, average_coefs)

  # Mean Regression coefficients
  lapply(res$results, average_corr)

