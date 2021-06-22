### Title:    Analysing results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-22

  ## Make sure we have a clean environment:
  rm(list = ls())

# Packages ----------------------------------------------------------------

  pack_list <- c("ggplot2",
                 "dplyr",
                 "forcats",
                 "stringr")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  runName <- "20212206_175047"

  # Read output
  sim_out <- readRDS(paste0(inDir, runName, "_res.rds"))
  gg_shape <- sim_out$gg_shape

  # Support Functions
  source("./helper/functions.R")

# Analyse -----------------------------------------------------------------

  # Possible Results
  names(sim_out$results[[1]]$rep1)

  # Mean result by name
  lapply(sim_out$results, average_result, result = "coefs")
  t(sapply(sim_out$results, average_result, result = "cors"))
  t(sapply(sim_out$results, average_result, result = "r2"))
  t(sapply(sim_out$results, average_result, result = "mses"))

# Plots -------------------------------------------------------------------

  ## Obtain plots
  result <- c("cors.", "mses.", "r2.")[2]
  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(result, variable)) %>%
    filter(D %in% unique(gg_shape$D)[c(-2)]) %>%
    filter(K %in% unique(gg_shape$K)) %>%
    # Get rid of useless cond info
    mutate(condTag = fct_relabel(condTag, str_replace, "^.*(?=(\\K))", "")
    ) %>%
    # Change labels of X axis
    mutate(variable = fct_relabel(variable, str_replace, result, "")
    ) %>%
    # Main Plot
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot() +
    # Grid
    facet_grid(rows = vars((D)),
               cols = vars(factor(K, unique(gg_shape$K))),
               scales = "free") +
    # Format
    ylim(0.35, .7) +
    labs(title = result,
         x     = NULL,
         y     = NULL)

  plot1
  
  png("~/Desktop/ggplot2.png")
  plot1
  dev.off() 
  
  