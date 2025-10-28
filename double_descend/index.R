# --- DOUBLE DESCENT SIMULATION IN R ---

set.seed(123)

# 1. Generate data
n <- 12               # number of observations
p_max <- 50           # maximum number of parameters
X <- matrix(rnorm(n * p_max), n, p_max)
true_beta <- rnorm(p_max)
y <- X %*% true_beta + rnorm(n, 0, 0.1)  # small noise

# 2. Test data
n_test <- 100
X_test <- matrix(rnorm(n_test * p_max), n_test, p_max)
y_test <- X_test %*% true_beta + rnorm(n_test, 0, 0.1)

# 3. Compute test error for different numbers of parameters
test_errors <- numeric(p_max)

for(p in 1:p_max) {
  X_sub <- X[, 1:p, drop=FALSE]
  X_test_sub <- X_test[, 1:p, drop=FALSE]
  
  if(p <= n) {
    # Normal linear regression
    beta_hat <- solve(t(X_sub) %*% X_sub) %*% t(X_sub) %*% y
  } else {
    # Over-parameterized: use minimum-norm solution (Moore-Penrose pseudoinverse)
    beta_hat <- MASS::ginv(X_sub) %*% y
  }
  
  y_pred <- X_test_sub %*% beta_hat
  test_errors[p] <- log(mean((y_test - y_pred)^2))
}

# 4. Plot results
plot(1:p_max, test_errors, type="b", pch=19,
     xlab="Number of parameters", ylab="Test MSE",
     main="Double Descent Phenomenon")
abline(v=n, col="red", lty=2)  # interpolation threshold
text(n+0.5, max(test_errors), "Interpolation Threshold", pos=4, col="red")
