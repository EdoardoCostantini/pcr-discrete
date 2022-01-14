# Project:   pcr_discrete
# Objective: Test script for generateDV.R
# Author:    Edoardo Costantini
# Created:   2022-01-14
# Modified:  2022-01-14

context("generateXTP")

# Set up of test
set.seed(20220114)

# Define target parameters
N <- 1e3
P <- 9
K <- 3
CPVE <- 0.9

# Use function
XTP <- generateXTP(I = N,
                   J = P,
                   R = K,
                   CPVE = CPVE)

# Perform PCA
PCA <- prcomp(XTP$X)

# Extract values to check correct recovery
# Eigen values
eigen_svd <- round(prop.table(svd(XTP$X)$d), 3)
eigen_pca <- round(prop.table(PCA$sdev), 3)

# Component Scores
T_svd <- round(svd(XTP$X)$u %*% diag(svd(XTP$X)$d), 3)
T_pca <- round(PCA$x, 3)

# Loadings
P_svd <- round(svd(XTP$X)$v, 3)
P_pca <- round(PCA$rotation, 3)

# Tests
test_that("Component scores obtained with SVD and PCA are the same", {
  expect_true(all.equal(T_svd, T_pca, check.attributes = FALSE), 1)
})
test_that("Eigen values obtained with SVD and PCA are the same", {
  expect_true(all.equal(eigen_svd, eigen_pca, check.attributes = FALSE), 1)
})
test_that("Loadings obtained with SVD and PCA are the same", {
  expect_true(all.equal(P_svd, P_pca, check.attributes = FALSE), 1)
})