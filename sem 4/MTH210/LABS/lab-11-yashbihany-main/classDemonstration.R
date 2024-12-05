################################################
## Old Faithful Geyser data
################################################
data(faithful)
head(faithful)

x <- faithful$eruptions
hist(x, breaks = 30, main = "Eruptions")


################################################
## EM Algorithm for the Old Faithful Geyser data
################################################
## Estep gamma calculation
## this is a general function that we are not actuall
gamma_ick <- function(x, mu, sig2, pis, C = 2)
{
  gamma_prob <- matrix(0, nrow = length(x), ncol = C)
  for(c in 1:C)
  {
    gamma_prob[ ,c]  <- dnorm(x, mean = mu[c], sd = sqrt(sig2[c]))* pis[c]
  }
  
  gamma_prob <- gamma_prob/(rowSums(gamma_prob))
  return(gamma_prob)
}


# Starting values
pis <- c(.6, .4) 
mu <- c(1, 5)
sig2 <- c(1, 2)
diff <- 100
tol <- 1e-5
iter <- 0

# just for visualizing
current <- c(pis, mu, sig2)
store <- current
C <- 2
while(diff > tol)
{
  previous <- current
  iter <- iter + 1
  
  # E step: find gamma_{i,c,k} for just c = 1, since for c = 2 is just 1-Ep
  # Ep <- current[1]*dnorm(x, current[2], sqrt(current[4]))/
  #   (current[1]*dnorm(x, current[2], sqrt(current[4])) + (1 - current[1])*dnorm(x, current[3], sqrt(current[5])))
  # 
  Ep <- gamma_ick(x, mu, sig2, pis, C = 2)
  
  # M-step
  pis <- colMeans(Ep)
  mu <- colSums(Ep*x) / colSums(Ep)
  for(c in 1:C)
  {
    sig2[c] <- sum(Ep[,c]*(x - mu[c])^2) / sum(Ep[,c])
  }
  current <- c(pis, mu, sig2)
  
  diff <- norm(previous - current, "2")
  store <- rbind(store, current)
}

current # final estimates


# Final estimates of the probability
# that each observation is in Class C.
Prob.Z <- gamma_ick(x, mu = mu, sig2 = sig2, pis = pis)

head(round(Prob.Z, 10))


# Make plot of iterative model fits
hist(x, breaks = 30, main = "Eruptions", freq = FALSE)
for(i in 1:dim(store)[1])
{
  test.x <- seq(min(x), max(x), length = 1000)
  test.y <- store[i,1]* dnorm(test.x, mean = store[i,3], sd = sqrt(store[i,5])) + (store[i,2]) *dnorm(test.x, mean = store[i,4], sd = sqrt(store[i,6]))
  lines(test.x, test.y, col = rgb(1,0,0, alpha = .5))
  i <- i + 1
}
lines(test.x, test.y, col = rgb(0,0,1, alpha = 1))

# add color
color <- apply(Prob.Z, 1, which.max)
points(x, rep(0, length(x)), pch = 16, col = color)
