rm(list = ls())
set.seed(123)

source("GKFP1.R")

data(iris)
X <- as.matrix(iris[, 1:4])

res <- GKFP1(X = X, c = 3, m = 2, zeta = 0.5)

print(res$V)
print(res$M)

cat("Test completed successfully.\n")

