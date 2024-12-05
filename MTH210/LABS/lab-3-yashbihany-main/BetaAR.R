#########################
## Accept-reject for
## Beta(4,3) distribution
## Using U(0,1) proposal
#########################
set.seed(1)
## taking uniform(0,1) as the proposal
beta_ar <- function() 
{
  c <- 60 *(3/5)^3 * (2/5)^2 
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1

    U <- runif(1)
    prop <- runif(1)
    ratio <- 60*(prop)^(3)*(1-prop)^(2)/c
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
  rep <-  beta_ar()
  samp[i] <-  rep[1]
  counts[i] <- rep[2]
}

# Make a plot of the estimated density from the samples
# versus the true density
x <- seq(0, 1, length = 500)
par(mfrow = c(1,1))
plot(density(samp), main = "Estimated density from 1e4 samples")
lines(x, dbeta(x, 4, 3) , col = "red", lty = 2) ## Complete this
legend("topleft", lty = 1:2, col = c("black", "red"), legend = c("AR", "truth"))

# This is c
(c <- 60 *(3/5)^3 * (2/5)^2)

# This is the mean number of loops required
mean(counts)

#They should be almost the same!


## question 2

#X be an conditional Exp(1)

density_x_cond <- function(x)
{
  return(exp(-x)/(1 - exp(-0.05)))
}

x_inv_transform <- function(x)
{
  return(-log(1 - (1 - exp(-0.05))*u))
}

N <- 1e3
u <- runif(N)
exp <- mean(x_inv_transform(u))
exp

## Question 5 : Beta(2, .1)

beta_ar <- function() 
{
  c <- (gamma(2.1)/(gamma(2)*gamma(0.1)*0.1))
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    
    U <- runif(1)
    prop_temp <- runif(1)
    prop <- (1 - (1-prop_temp)^10)
    ratio <- prop
    if(U <= ratio)
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}
N <- 1e4
samp <- replicate(N, beta_ar()[1])
counts <- replicate(N, beta_ar()[2])

plot(density(samp), main = "Estimated beta(2, 0.1 from 1e4 samples")
x <- seq(0, 1, length = 500)
lines(x, dbeta(x, 2, 0.1), lty = 2, col = "red")


## question 6

## target = gamma( 4, 3) x in (0, inf)
## proposal = exp(lambda) x in (0, inf)


