context("makeData")

# makeData generates X with desired PC structure

# Set up of test
set.seed(20220105)
K <- 3 # number of components
J <- 9 # number of indicators
vars <- c(seq(10, 5, length.out = K),
          sort(runif(J - K, min = 0.01, max = 0.5), decreasing = TRUE))

dat   <- makeDat(n = 1e4,
                 variances = vars)

# Similar Eigen vlaues
eign_true <- round(vars, 3)
eign_data <- round(eigen(cov(dat$X))$values, 3)

# Explained variances
PVE_true <- prop.table(vars)
PVE_data <- prop.table(eigen(cov(dat$X))$values)

# Tests
test_that("Target and data eigen values are similar", {
  expect_true(all(abs(eign_true - eign_data) < 1), 1)
  expect_true(all(abs(PVE_true - PVE_data) < 1), 1)
})