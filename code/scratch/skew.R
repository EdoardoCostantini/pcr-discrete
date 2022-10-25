
library(fungible)

set.seed(20221014)

# Internals
I    = 1e5 # sample size
J    = 9 # number of variables
VAFr = c(.5, .3, .2) # relative variance of each components
VAFsum = 100 # total variance of the components
CPVE = 0.9 # proportion of explained variance by the R components

# Number of components
R <- length(VAFr)

# Random sample U
# V1: monte1 function
# U <- monte1(
#     seed = NULL,
#     nsub = I,
#     nvar = R,
#     skewvec = c(-1, 0, 1),
#     kurtvec = rep(0, R),
#     cormat = diag(1, R)
# )$data

# # Check skewness
# apply(U, 2, skew)

# V2: Skew normal distribution
# Sample from the RSN based on Azzalini
alphas <- c(10, 5, 1) # values showcased by azzalini
U <- sapply(alphas, function(j) {
    rsn(n = I, xi = 0, omega = 1, alpha = j, tau = 0)
})

# Check skewnees produced
apply(U, 2, e1071::skewness)

# Scale and continue with regulard steps
U <- scale(U, center = TRUE, scale = FALSE)
U <- orthmat(U, verbose = FALSE)
U <- normmat(U)

# Random sample P
V <- matrix(
    data = runif(J * R),
    nrow = J,
    ncol = R
)
V <- orthmat(V, verbose = FALSE)
P <- normmat(V)

# Define D
D <- diag(c(VAFsum * VAFr))

# Create X
Xtrue <- U %*% D %*% t(P)

# Scale for visualization
Xtrue <- scale(Xtrue)

plot(density(Xtrue[, 9]), ylim = c(0, .8))
sapply(2:9, function(j) lines(density(Xtrue[, j])))

# Estimate 3 pcs and check how they look
# SVD decomposition
svd_out <- svd(Xtrue)

# Compute the PC scores
T <- (svd_out$u %*% diag(svd_out$d))

# Compute a vector of cumulative proportions of explained variances
CPVE <- cumsum(prop.table(svd_out$d^2))
CPVE

# Store true components
Tt <- U %*% D

# Check correlations with true and untrue components
cor(Tt[, 1], T[, 1])
cor(Tt[, 2], T[, 2])
cor(Tt[, 3], T[, 3])

# Plot true components
plot(density(Tt[, 1]), ylim = c(0, 25))
sapply(2:R, function(j) lines(density(Tt[, j])))

# Is this good recovery?
plot(density(T[, 1]), ylim = c(0, 1.5))
sapply(2:R, function(j) lines(density(T[, j])))

x <- fGarch::rsnorm(1e4, mean = 0, sd = 1, xi = 10)
skew(x)
plot(density(fGarch::rsnorm(1e4, mean = 0, sd = 1, xi = 1.5))

# Check resulting item skewness -----------------------------------------------

    set.seed(1235)

    # Generate data
    XTP <- generateXTP(
        I = 1e4,
        J = parms$P,
        VAFr = parms$XTP_VAFr,
        VAFsum = parms$XTP_VAFsum,
        CPVE = .9,
        skewness = c(0, -2, 2),
        kurtosis = c(0, 10, 10)
    )

    # Store items skewness results
    apply(XTP$T, 2, e1071::skewness)
    svd_out <- svd(XTP$X)
    T <- (svd_out$u %*% diag(svd_out$d))
    apply(T[, 1:3], 2, e1071::skewness)
    sort(apply(XTP$X, 2, e1071::skewness))
    order(XTP$skew)

    # Graphs
    par(mfrow = c(1, 3))
    apply(XTP$T, 2, function(x) plot(density(x)))

    par(mfrow = c(3, 4))
    apply(XTP$X[, order(XTP$skew)], 2, function(x) plot(density(x)))

# Overcome skewed normal distribution limit of gamma < 1 ----------------------

# The SKewed normal has the limit of gamma < 1

# This works
cp <- c(0, 1, .8)
dp <- cp2dp(cp, family = "SN")
x <- rsn(1e5, dp = dp)
e1071::skewness(x)
e1071::kurtosis(x)

# But this does not
cp <- c(0, 1, 2)
dp <- cp2dp(cp, family = "SN")

# So we move to the Skewed t distribution which allows changing the excess kurtosis
cpST <- c(0, 1, 2, 10)
dpST <- cp2dp(cpST, family = "ST")
x <- rst(1e6, dp = dpST)
plot(density(x))
e1071::skewness(x)
e1071::kurtosis(x)

# Note a normal distribution with kurtosis
cpST <- c(0, 1, 0, 0)
dpST <- cp2dp(cpST, family = "ST")
x <- rst(1e5, dp = dpST)
e1071::skewness(x)
e1071::kurtosis(x)

z <- rnorm(1e5)
e1071::skewness(z)
e1071::kurtosis(z)

plot(density(x))
lines(density(rnorm(1e4)))

# Push it even more
cpST <- c(0, 1, 3, 40)
dpST <- cp2dp(cpST, family = "ST")
x <- rst(1e6, dp = dpST)
plot(density(y2))
e1071::skewness(x)
e1071::kurtosis(x)
