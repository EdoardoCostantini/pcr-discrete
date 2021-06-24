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
  runName <- "20212306_112142"

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
    filter(D %in% unique(gg_shape$D)) %>%
    filter(K %in% unique(gg_shape$K)) %>%
    filter(value < 1) %>% # to get rid of some outliers
    # Change labels of X axis
    mutate(variable = fct_relabel(variable, str_replace, result, "")
    ) %>%
    # Main Plot
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot() +
    # Grid
    facet_grid(rows = vars(factor(D,
                                  labels = paste0("D = ", unique(gg_shape$D)))),
               cols = vars(factor(K,
                                  levels = unique(gg_shape$K),
                                  labels = paste0("K = ", unique(gg_shape$K)))),
               scales = "free") +
    # Format
    theme(text = element_text(size = 12.5),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 12.5),
          axis.title = element_text(size = 12.5)) +
    labs(title = result,
         x     = NULL,
         y     = NULL)

  plot1
  
  png("~/Desktop/ggplot2.png")
  plot1
  dev.off() 
  
  