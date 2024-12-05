###############
# Demo on Ridge Regression 
###############
set.seed(1)
n <- 50
p <- 1
beta.star <- c(4)
beta.star  # to output

# Making design matrix
X <- matrix(rnorm(n*p), nrow = n, ncol = p)

# Generating response
y <-  X %*% beta.star + rnorm(n, mean = 0, sd = 1)

# -ve log likelihood
negLogLik <- function(y, X, beta)
{
  t(y - X %*% beta) %*% (y - X %*% beta)/2
}

ridgePen <- function(beta, lam)
{
  lam * t(beta) %*% beta/2
}

grid <- 5e2
betas <- seq(-2, 10, length = grid)
nll <- numeric(length = 5e2)
pen <- numeric(length = 5e2)
ridge <- numeric(length = 5e2)
lam <- 1e1
for(i in 1:grid)
{
  nll[i] <- negLogLik(y, X, beta = betas[i])
  pen[i] <- ridgePen(beta = betas[i], lam = lam)
  ridge[i] <- nll[i] + pen[i]
}

plot(betas, nll, type = 'l', ylim = range(c(nll, ridge, pen)),
     xlab = expression(beta), ylab = "Objective function")
points(x = betas[which.min(nll)], y = min(nll), pch = 16)
lines(betas, pen, col = "red")
lines(betas, ridge, col = "blue")
points(x = betas[which.min(ridge)], y = min(ridge), pch = 16, col = "blue")
legend("topleft", col = c("black", "red", "blue"), lty = 1,
       legend = c("Neg Log Likelihood", "Penaltry term", "Ridge Objective Fn"))





####################################################
### MLE for Gamma(alpha, 1)
####################################################
set.seed(100)
library(pracma)  #for psi function

####################################################
# riginal data sample size is small first
# The NR methods estimates the MLE. Here the 
# blue and red lines will not match because
# the data is not large enough for the consistency of 
# the MLE to kick in.


alpha <- 5 #true value of alpha
n <- 10 # actual data size is small first
dat <- rgamma(n, shape = alpha, rate = 1)

alpha_newton <- numeric()
epsilon <- 1e-8  #some tolerance level preset
alpha_newton[1] <- 2  #alpha_0
count <- 1
tol <- 100 # large number
while(tol > epsilon)
{
  count <- count + 1
  
  #first derivative
  f.prime <- -n*psi(k = 0, alpha_newton[count - 1]) + sum(log(dat))
  
  #second derivative
  f.dprime <- -n*psi(k = 1, alpha_newton[count - 1])
  alpha_newton[count] <- alpha_newton[count - 1] - f.prime/f.dprime
  tol <- abs(alpha_newton[count] - alpha_newton[count-1])
}
alpha_newton

#Plot the log.likelihood for different values of alpha
alpha.grid <- seq(0, 10, length = 100)
log.like <- numeric(length = 100)
for(i in 1:100)
{
  log.like[i] <- sum(dgamma(dat, shape = alpha.grid[i], log = TRUE))
}
plot(alpha.grid, log.like, type = 'l', xlab = expression(alpha), ylab = "Log Likelihood")
abline(v = alpha, col = "red", lty = 2)
for(t in 1:count)
{
  points(alpha_newton[t], sum(dgamma(dat, shape = alpha_newton[t], log = TRUE)), pch = 16)
}
abline(v = tail(alpha_newton[count]), col = "blue", lty = 2)
legend("bottomright", legend = c("Likelihood", "Truth", "MLE"), lty = c(1,2,2), col = c("black", "red", "blue"))






####################################################
# Increasing original data sample size.
# Now the MLE is closer to the "truth"
# and our NR method obtains the MLE.
# Blue and red lines should match a lot

# Randomly generate data
alpha <- 5 #true value of alpha
n <- 1000 # actual data size is small first
dat <- rgamma(n, shape = alpha, rate = 1)
alpha_newton <- numeric()
epsilon <- 1e-8  #some tolerance level preset
alpha_newton[1] <- 2  #alpha_0
count <- 1
tol <- 100 # large number
while(tol > epsilon)
{
  count <- count + 1
  f.prime <- -n*psi(k = 0, alpha_newton[count - 1]) + sum(log(dat))
  f.dprime <- -n*psi(k = 1, alpha_newton[count - 1])
  alpha_newton[count] <- alpha_newton[count - 1] - f.prime/f.dprime
  tol <- abs(alpha_newton[count] - alpha_newton[count-1])
}
alpha_newton

#Plot the log.likelihood for different values of alpha
alpha.grid <- seq(0, 10, length = 100)
log.like <- numeric(length = 100)
for(i in 1:100)
{
  log.like[i] <- sum(dgamma(dat, shape = alpha.grid[i], log = TRUE))
}
plot(alpha.grid, log.like, type = 'l', xlab = expression(alpha), ylab = "Log Likelihood")
abline(v = alpha, col = "red", lty = 2)
for(t in 1:count)
{
  points(alpha_newton[t], sum(dgamma(dat, shape = alpha_newton[t], log = TRUE)), pch = 16)
}
abline(v = tail(alpha_newton[count]), col = "blue", lty = 2)
legend("bottomright", legend = c("Likelihood", "Truth", "MLE"), lty = c(1,2,2), col = c("black", "red", "blue")) 

