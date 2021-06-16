### Title:    Analysing results
### Project:  Ordinality
### Author:   Edoardo Costantini
### Created:  2021-06-10
### Modified: 2021-06-14

  ## Make sure we have a clean environment:
  rm(list = ls(all = TRUE))

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  runName <- "20211606_124954"

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
  sim_out$conds
  store <- as.data.frame(vector("list", 9))
  for (i in 1:nrow(sim_out$conds)){
    for(r in 1:sim_out$parms$dt_rep){
      content <- data.frame(cond = sim_out$conds$tag[i],
                            mses = as.data.frame(t(sim_out$results[[i]][[r]]$mses)),
                            r2 = as.data.frame(t(sim_out$results[[i]][[r]]$r2)),
                            cors = as.data.frame(t(sim_out$results[[i]][[r]]$cors))
      )
      store <- rbind(store, content)
    }
  }

  gg_shape <- reshape2::melt(store,
                             id.var = c("cond"))
  head(gg_shape)

  ## Obtain plots
  result <- c("cors.", "mses.", "r2.")[1]
  
  plot1 <- gg_shape %>%
    filter(grepl(result, variable)) %>%
    # Get rid of useless cond info
    mutate(cond = fct_relabel(cond,
                              str_replace,
                              "^.*(?=(\\K))", "")
    ) %>%
    # Get better order
    mutate(cond = fct_relevel(cond,
                              "K10", "K7", "K5", "K3", "K2")
    ) %>%
    # Change labels of X axis
    mutate(variable = fct_relabel(variable,
                                  str_replace,
                                  result, "")
    ) %>%
    # Main Plot
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot() +
    # Grid
    facet_grid(cols = vars(cond),
               scales = "free") +
    # Format
    labs(title = result,
         x     = NULL,
         y     = NULL)
  
  plot1
