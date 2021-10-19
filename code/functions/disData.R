# Project:   ordinality
# Objective: Function to discretize all the columns of dataset
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2021-10-19

disData <- function(x, K, interval = TRUE){
  # Given a continuous varible x, and a number of categories K,
  # this function return a discretized version (either ordinal or
  # interval scale)

  ## Example inputs
  # x = rnorm(1e3)
  # K = 3
  # interval = TRUE

  if (interval == TRUE){
    x_dis <- as.numeric(cut(x, K))
  } else {
    prob_reduction <- .6 # every subsequent bin contains .6 of the remaining obs
    prob_in <- .2 # first bin probability
    probs <- rep(NA, K)

    for(k in 1:K){
      if(k < K){
        probs[k] <- prob_in
        whats_left <- (1-sum(probs, na.rm = TRUE))
        prob_in <- whats_left*prob_reduction
      } else {
        probs[k] <- whats_left
      }
    }
    x_sort <- sort(x)
    x_mem <- sort(sample(1:K, length(x), probs, replace = TRUE))
    map <- data.frame(value = as.character(x_sort),
                      bin = x_mem)
    target <- as.character(x)
    x_dis <- map[match(target, map$value), "bin"]
  }
  return(x_dis)
}
