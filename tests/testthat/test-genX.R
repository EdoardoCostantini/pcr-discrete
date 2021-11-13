context("genX")

# Try test function on all conditions

conds_test <- conds[, c("rho", "blocks", "skew", "kurt")]
conds_test <- conds_test[!duplicated(conds_test), ]

store_skw <- rep(NA, nrow(conds_test))
store_cor <- rep(NA, nrow(conds_test))
store_krt <- rep(NA, nrow(conds_test))

reps <- 30

set.seed(20211104)

for (i in 1:nrow(conds_test)){
  skewness <- rep(NA, reps)
  kurtosis <- rep(NA, reps)
  correlation <- rep(NA, reps)
  for (r in 1:reps){
    x <- genX(parms = parms, cond = conds_test[i, ])
    skewness[r] <- skewness(x[, 1])
    kurtosis[r] <- kurtosis(x[, 1])
    correlation[r] <- cor(x)[1, 2]
  }
  store_skw[i] <- mean(skewness)
  store_krt[i] <- mean(kurtosis)
  store_cor[i] <- mean(correlation)
}

round(cbind(rho = conds_test$rho, rho_emp = store_cor,
            skw = conds_test$skew, skw_emp = store_skw,
            krt = conds_test$kurt, krt_emp = store_krt), 0)

test_that("Functions produces expected cor, skew, and kurt", {
  expect_equal(conds_test$rho, store_cor, 1)
  expect_equal(conds_test$skew, store_skw, 1)
  expect_equal(conds_test$kurt, store_krt, 1)
})