### Title:    Analysing results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-14

  ## Make sure we have a clean environment:
  rm(list = ls(all = TRUE))

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  runName <- "20212206_151918"

  # Extract last element as session info object
  sim_out <- readRDS(paste0(inDir, runName, "_res.rds"))

  # Support Functions
  source("./helper/functions.R")

# Analyse -----------------------------------------------------------------

  # Possible Results
  names(sim_out$results$N1000_L8_J3_P24_fl0.8_K2$rep1)

  # Mean result by name
  lapply(sim_out$results, average_result, result = "coefs")
  t(sapply(sim_out$results, average_result, result = "cors"))
  t(sapply(sim_out$results, average_result, result = "r2"))
  t(sapply(sim_out$results, average_result, result = "mses"))

# Plots -------------------------------------------------------------------
  
  ## Packages for plots 
  library(ggplot2)
  library(dplyr)
  library(forcats)
  library(stringr)

  ## Shape data for plot
  sim_out$conds$D
  store <- as.data.frame(vector("list", 11))
  for (i in 1:nrow(sim_out$conds)){
    # i <- 1
    for(r in 1:sim_out$parms$dt_rep){
      # r <- 1
      content <- data.frame(condTag = sim_out$conds$tag[i],
                            K = sim_out$conds$K[i],
                            D = sim_out$conds$D[i],
                            mses = as.data.frame(
                              t(
                                sqrt(
                                  sim_out$results[[i]][[r]]$mses
                                )
                              )
                            ),
                            r2 = as.data.frame(
                              t(
                                sim_out$results[[i]][[r]]$r2
                              )
                            ),
                            cors = as.data.frame(
                              t(
                                sim_out$results[[i]][[r]]$cors
                              )
                            )
      )
      store <- rbind(store, content)
    }
  }
  
  gg_shape <- reshape2::melt(store,
                             id.var = c("condTag", "K", "D"))
  
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
    labs(title = result,
         x     = NULL,
         y     = NULL)

  # plot1
  
  png("~/Desktop/ggplot2.png")
  plot1
  dev.off() 
  
  