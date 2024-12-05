
###########################################
## Pseudo-random number generation
## using multiple congruential method
###########################################
m <-   # choose a value you want
a <-   # choose a value you want
x <- numeric(length = 1e3)
x[1] <-   #x0 -- choose
for(i in 2:1e3)
{
  x[i] <- (a * x[i-1]) %% m
}


# We will visualize using histograms (for testing uniformity)
# and trace plots (for checking independence)
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too

