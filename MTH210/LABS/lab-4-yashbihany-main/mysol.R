## 1 - Beta(2, .1)
beta_ar <- function(a = 2, b = 0.1)
{
  ## proposal = b*(1 - x)^(b - 1)
  c <- gamma(a + b)/(gamma(a) * gamma(b) * b)
  itr <- 0
  foo <- 0

  while(foo == 0)
  {
    itr <- itr + 1
    u <- runif(1)
    temp <- (1/ b) * log(u)
    y <- 1 - exp(temp)
    
    ratio <- exp((a - 1)*log(y))
    v <- runif(1)
    if(v <= ratio)
    {
      foo <- 1
      return(c(y, itr))
    }
  }
  
}
N <- 1e4
samp <- numeric(N)
counts <- numeric(N)
for(i in 1:N)
{
  rep <- beta_ar()
  samp[i] = rep[1]
  counts[i] = rep[2]
}


x <- seq(0, 1, length = 5000)
plot(density(samp), main = "estimated denstity from 1e4 samples")
lines(x, dbeta(x, 2, 0.1), col = "red", lty = 2)
legend("topleft", lty = 1: 2, col = c("black", "red"), legend = c("AR", "truth"))


## Answer 2

gamma_ar <- function(a = 4, b = 3, l = 3/ 4)
{
  ## proposal = exp(l)
  x.max <- (a - 1)/ (b - l)
  c <- dgamma(x.max, a, b)/ dexp(x.max, l)
  
  acc <- 0
  itr <- 0
  while(acc == 0)
  {
    itr <- itr + 1
    u <- runif(1)
    prop <- (-1/ l)*log(u)
    log.ratio <- dgamma(prop, a, b, log = TRUE) - log(c) - dexp(prop, l, log = TRUE)
    v <- runif(1)
    if(log(v) <= log.ratio)
    {
      acc <- 1
      return(c(prop, itr))
    }
  }
}

N <- 1e4
samp <- numeric(N)
counts <- numeric(N)
for(i in 1:N)
{
  rep <- gamma_ar()
  samp[i] <- rep[1]
  counts[i] <- rep[2]
}
mean(counts)

plot(density(samp), ylab = "Density", main = "Average iterations = 2.1")
x <- seq(0, 6, length = 1e3)
lines(x, dgamma(x, 4, 3), col = "red")


## for lambda = 2
N<- 1e4
samp <-numeric(N)
counts <- numeric(N)
for(i in 1:N)
{
  rep <- gamma_ar(4,3,l = 2)
  samp[i] <- rep[1]
  counts[i] <- rep[2]
}
mean(counts)

plot(density(samp), ylab=  "Density", main = "Average iterations = 9")
x <- seq(0, 6, length = 1e3)
lines(x , dgamma(x , 4, 3), col=  "red")


## answer 3

## for N(0, 1) <- cauchy proposal 
## solution pdf