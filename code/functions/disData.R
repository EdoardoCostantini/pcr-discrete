# Project:   ordinality
# Objective: Function to discretize all the columns of dataset
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2022-01-21

disData <- function(x, K, interval = TRUE){
  # Given a continuous varible x, and a number of categories K,
  # this function return a discretized version (either ordinal or
  # interval scale)

  ## Example inputs
  # x = rnorm(1e3)
  # K = 2
  # interval = TRUE

  if (interval == TRUE){
    x_dis <- as.numeric(cut(x, K))
  } else {
    # Define a starting probability for the first bin
    prob_in <- .4 # first bin probability

    # Define a reduction probability for subsequent bins
    prob_reduction <- .6 # every subsequent bin contains .6 of the remaining obs

    # Define an empty vector to store the probabilities of being in a bin
    probs <- rep(NA, K)

    # Compute the probabilities
    for(k in 1:K){
      if(k < K){
        probs[k] <- prob_in
        whats_left <- (1 - sum(probs, na.rm = TRUE))
        prob_in <- whats_left * prob_reduction
      } else {
        probs[k] <- whats_left
      }
    }

    # Define the break points for the desired bins
    breaks <- cumsum(c(0, probs))

    # Obtain the cdf values for the given x
    cdf_x <- pnorm(x, mean = mean(x), sd = sd(x))

    # Cut the cdf_x with the breaks
    x_dis <- as.numeric(cut(cdf_x, breaks))
  }
  return(x_dis)
}
