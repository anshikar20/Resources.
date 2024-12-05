
###########################################
## Pseudo-random number generation
## using multiple congruential method
###########################################
m <-   2^(31) - 1
a <-   7^5
x <- numeric(length = 1e3)
x[1] <-   3
for(i in 2:1e3)
{
  x[i] <- (a * x[i-1]) %% m
}

c <- 13
for(i in 2: 1e3)
{
  x[i] <- (a*x[i-1] + c)%%m
}

# We will visualize using histograms (for testing uniformity)
# and trace plots (for checking independence)
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too

myBern <- function(p)
{
  x <- runif(1)
  if(x < 1-p)return(0)
  else return(1)
}
p <- 0.25
test <- numeric(length = 1e3)
for(i in 1:1e3)test[i] = myBern(p)
## or simply
out <- replicate(1e3, myBern(p))
test
plot(test)
sum(test)/length(test)
mean(out)


mySample <- function(x, size = 1, prob = rep(1/length(x), length(x)))
{
  myvec <- numeric(length = size)
  for(j in 1:size)
  {
    t <- runif(1)
    i <- 1
    sum <- prob[1]
    while(i <= length(x) && t > sum)
    {
      i <- i + 1
      sum <- sum + prob[i]
    }
    myvec[j] = x[i]
  }
  
  return(myvec)
}
mySample(x = c(3,6,9), size = 1, prob = c(.3, .4, .3))
samples <- mySample(x = c(3,6,9), size = 1e4, prob = c(.3, .4, .3))
table(samples)/1e4
