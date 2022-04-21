# Project:   ordinality
# Objective: Function to discretize all the columns of dataset
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2022-04-21

disData <- function(x, K, interval = TRUE, min_bin = 0.05){
  # Given a continuous varible x, and a number of categories K,
  # this function return a discretized version (either ordinal or
  # interval scale)

  ## Example inputs
  # x = rnorm(1e3)  # variable to discretize
  # K = 3           # number of target categories
  # interval = TRUE # whether we are discretizing to interval (T) or orindal (F)
  # min_bin = 0.05  # minimum proportion of cases in a bin for ordinal items

  if (interval == TRUE){
    # Define vector of lags (equally spaced)
    lags <- rep(abs(min(x) - max(x)) / K, (K-1))

    # Define the break points x
    breaks <- c(cumsum(c(minimum = min(x), fixed = lags)), maximum = max(x))

    # Cut x with the given brakes
    x_dis <- as.numeric(cut(x = x, breaks = breaks, include.lowest = TRUE))

    # Compute proportion of cases in each bin
    prop_cases <- table(x_dis)/length(x)
  } else {

    # Define a starting probability for the first bin
    prob_in <- .6 # first bin probability

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
    breaks_p <- cumsum(c(0, probs))
    breaks <- qnorm(breaks_p, mean = mean(x), sd = sd(x))

    # Cut x with the given brakes
    x_dis <- as.numeric(cut(x = x, breaks = breaks, include.lowest = TRUE))

    # Compute proportion of cases in each bin
    prop_cases <- table(x_dis)/length(x)

  }
  return(list(x = x_dis,
              breaks = breaks,
              prop_cases = prop_cases))
}
