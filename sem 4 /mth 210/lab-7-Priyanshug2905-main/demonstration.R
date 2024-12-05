###########################################################
## This demonstration show the behavior of the likelihood
## as n increases and the behavior of the MLE
## as n increases
###########################################################

# setting the true value of theta
theta.star <- 5

theta <- seq(0, 10, length = 1e3)
reps <- 1e2  # repeat experiment multiple times

# Save likelihood function evaluations
likes.onedata <- matrix(0, nrow = reps, ncol = 1e3)
likes.hunddata <- matrix(0, nrow = reps, ncol = 1e3)

# save MLEs
mle1 <- numeric(length = reps)
mle100 <- numeric(length = reps)

for(r in 1:reps)
{
  # get one data point and evaluate likelihood function
  data1 <- rnorm(1, mean = theta.star)
  likes.onedata[r, ] <- dnorm(data1, mean = theta, sd = 1, log = TRUE)
  mle1[r] <- data1
  
  # get 100 data points and evaluate likelihood function over grid of values of theta
  data100 <- rnorm(100, mean = theta.star)
  likes.hunddata[r, ] <- rowSums(sapply(data100, dnorm, mean = theta, log = TRUE))/100
  mle100[r] <- mean(data100)  # MLE is just sample mean
}



plot(theta, likes.onedata[1,], type = "n", ylim = range(cbind(likes.onedata, likes.hunddata)), main = "Variability of the MLE from Sample size 1", 
     ylab = "Expected Log-likelihood", xlab = expression(theta))
for(r in 1:reps)
{
  lines(theta, likes.onedata[r,], col = adjustcolor("red", .2))
}
abline(v = mle1, col = adjustcolor("orange", .5))
## organge line represents MLE from each likelihood function
## Notice the variability in the MLE (how widescread orange line is)



## Redo this for 100 sample size
plot(theta, likes.hunddata[1,], type = "n", ylim = range(cbind(likes.onedata, likes.hunddata)), main = "N(5,1) Expected Log-likelihood", ylab = "Expected Log-likelihood")
for(r in 1:reps)
{
  lines(theta, likes.hunddata[r,], col = adjustcolor("blue", .2))
}
abline(v = mle100, col = adjustcolor("orange", .5))
## organge line represents MLE from each likelihood function again
## but now the maximas happen close together
## MLE has low variability. 
