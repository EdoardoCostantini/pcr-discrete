context("makeData")

# makeData generates X with desired PC structure

# Set up of test
set.seed(20220105)
J <- 9 # number of indicators
error <- .05

vs <- makeVariance(c(100, 80, 70), J = J, error = error)
dat   <- makeDat(n = 1e4,
                 Q = K,
                 vs = vs)

# Similar Eigen vlaues
eign_true <- round(vs, 3)
eign_data <- round(eigen(cov(dat$X))$values, 3)

# Explained variances
PVE_true <- prop.table(vars)
PVE_data <- prop.table(eigen(cov(dat$X))$values)
uPV <- sum(eign_data[-c(1:3)])/sum(eign_data) # unexplained pv

# Tests
test_that("Target and data eigen values are similar", {
  expect_true(all(abs(eign_true - eign_data) < 1), 1)
  expect_true(all(abs(PVE_true - PVE_data) < 1), 1)
})
test_that("Proportion of unexplained variance is equal to error", {
  expect_true(all(abs(uPV - error) < .005), 1)
})