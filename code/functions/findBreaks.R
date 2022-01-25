# Project:   pcr_discrete
# Objective: function to finds breaks to discretize a N(mu, std) var
# Author:    Edoardo Costantini
# Created:   2022-01-25
# Modified:  2022-01-25

findBreaks <- function(mu, std, K, interval) {
  # Internals -------------------------------------------------------------

  # mu = 0
  # std = 1
  # K = 7
  # interval = TRUE

  # Body ------------------------------------------------------------------
  # Get a random sample
  x <- rnorm(1e3, mu, std)

  # Find the breaks based on interval or not
  if (interval == TRUE){
    # Define vector of lags (equally spaced)
    lags <- rep(abs(min(x) - max(x)) / K, (K-1))

    # Define the break points x
    breaks <- c(cumsum(c(minimum = min(x), fixed = lags)), maximum = max(x))
  } else {
    # Define an indictor of status for a while loop
    continue <- TRUE

    # Start a loop to obtain a sufficiently varied discretized variable
    while (continue){
      # Define vector of random breaks
      breaks <- c(minimum = min(x),
                  random = sort(runif(n = (K-1), min = min(x), max = max(x))),
                  maximum = max(x))

      # Cut x with the given brakes
      x_dis <- as.numeric(cut(x = x, breaks = breaks, include.lowest = TRUE))

      # Compute proportion of cases in each bin
      prop_cases <- table(x_dis)/length(x)

      # If at least half of the categories have more than .1 cases, stop
      if(all(prop_cases > .1)){
        continue <- FALSE
      }
    }
  }
  return(breaks)
}