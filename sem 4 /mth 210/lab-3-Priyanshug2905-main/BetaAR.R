#########################
## Accept-reject for
## Beta(4,3) distribution
## Using U(0,1) proposal
#########################
set.seed(1)
beta_ar <- function() 
{
  c <- 60 *(3/5)^3 * (2/5)^2 
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1

    # fill in the lines of code here to implement
    # ...
    # ...

    if(U <= ratio)
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}


### Obtaining 10^4 samples from Beta() distribution
N <- 1e4
samp <- numeric(length = N)
counts <- numeric(length = N)
for(i in 1:N)
{
  rep <-   ## fill in
  samp[i] <-  ## fill in
  counts[i] <- ## fill in
}

# Make a plot of the estimated density from the samples
# versus the true density
x <- seq(0, 1, length = 500)
plot(density(samp), main = "Estimated density from 1e4 samples")
lines(   , col = "red", lty = 2) ## Complete this
legend("topleft", lty = 1:2, col = c("black", "red"), legend = c("AR", "truth"))

# This is c
(c <- 60 *(3/5)^3 * (2/5)^2)

# This is the mean number of loops required
mean(counts)

#They should be almost the same!



