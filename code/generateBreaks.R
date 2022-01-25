# Project:   pcr_discrete
# Objective: Script to generate batch of breaks for simulation
# Author:    Edoardo Costantini
# Created:   2022-01-25
# Modified:  2022-01-25

# Make sure we have a clean environment:
rm(list = ls())

# Initialize the environment:
source("./init.R")

# Define the conditions for which we need to do this
conds <- expand.grid(K = n_cate, # number of categories
                     interval = interval,
                     stringsAsFactors = TRUE)

# How many break sets do you want?
n_batch <- 10

# Storing objects
shelf <- vector("list", length(n_cate))

# Create required batches
for(i in 1:n_batch){
  for(k in 1:length(n_cate)){
    breaks <- findBreaks(
      mu = 0, # our data will be centered around 0 before discretization
      std = 1, # our data will be scaled to have sd 1 before discretization
      K = n_cate[k],
      interval = FALSE # we want to generate these breaks for this condition
    )
    shelf[[k]] <- rbind(shelf[[k]], breaks)
  }
}

# Store them in the folder
saveRDS(shelf, "../input/breaks_batch.rds")