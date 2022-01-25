# Project:   pcr_discrete
# Objective: Test discretization function
# Author:    Edoardo Costantini
# Created:   2022-01-22
# Modified:  2022-01-22

context("disData")

# Set seed
set.seed(20220105)

# Generate some X
n <- 1e3
K <- c(2, 3, 5, 7)
p <- length(K)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# Non-interval scales
disData_out <- lapply(1:length(K), function (k) {
  disData(X[, k], K = K[k], interval = FALSE)}
)

# Keep breaks
breaks <- lapply(disData_out, "[[", "breaks")

# Keep variables
xs <- lapply(disData_out, "[[", "x")

# Keep props
prop_cases <- lapply(disData_out, "[[", "prop_cases")

# Visualize
  par(mfrow = c(2, 4))

  # Continuous variables with random breaks
  lapply(1:p, function(j){
    plot(density(X[, j]),
         main = paste0("X", j),
         xlab = "")
    abline(v = breaks[[j]], col = "gray")
    points(x = breaks[[j]], col = "gray",
           y = rep(0, length(breaks[[j]])))
  })

  # Densities of categorical variables
  lapply(1:p, function(j){
    plot(density(xs[[j]]),
         main = paste0("X", j),
         xlab = "")
  })

# Tests
test_that("Every category contains at least .1 of the data", {
  expect_true(all(unlist(prop_cases) > .1), 1)
})

# Use of the function

# Read batch of breaks to use
breaks_batch <- readRDS("../input/breaks_batch.RDS")

# Set seed
set.seed(20220105)

# Generate some X
n <- 1e3
K <- c(2, 3, 5, 7)
p <- length(K)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# Non-interval scales
disData_out <- lapply(1:length(K), function (k) {
  batch <- breaks_batch[[k]]
  current_breaks <- batch[sample(1:nrow(batch), size = 1), ]
  current_breaks[1] <- -Inf
  current_breaks[length(current_breaks)] <- +Inf

  disData(X[, k],
          K = K[k],
          interval = FALSE,
          breaks = current_breaks)
}
)
