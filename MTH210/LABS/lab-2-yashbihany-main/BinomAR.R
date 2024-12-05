###########################################
## Accept Reject algorithm to draw from
## Binomial(n,p)
###########################################
# setting the seed makes it so that the same sets of
# random variables are realized.
set.seed(1)  

# Function draws one value from Binom(n,p)
# n = number of trials
# p = probability of success
draw_binom <- function(n, p)
{
  accept <- 0 # Will track the acceptance
  try <- 0 # Will track the number of proposals
  
  # upper bound calculated in the notes
  x <- 0:n
  all_c <- choose(n,x) * (1-p)^(n - 2*x) * p^(x-1)  # from notes
  c <- max(all_c)
  
  while(accept == 0)
  {
    try <- try + 1

    U <- runif(1)
    prop <- rgeom(1, prob = p) #draw proposal
    
    ratio <-  choose(n, prop) * (1-p)^(n - 2*prop) * p^(prop - 1)/c
    if(U < ratio)
    {
      accept <- 1
      rtn <- prop
    }
  }
  return(c(rtn, try))
}
draw_binom(n = 10, p = .25)


###
# If we want X1, ..., Xn ~ Binom(n.p)
# we need to call the function multiple times

# sample size
N <- 1e3
samp <- numeric(N)
n.try <- numeric(N)
for(t in 1:N)
{
   # I use as a dummy variable often
   foo <- draw_binom(n = 10, p = .25)
   samp[t] <- foo[1]
   n.try[t] <- foo[2]
}
mean(samp) #should be n*p = 2.5
mean(n.try)


###########################################
## A closer look at Binomial and Geometric
###########################################
# Turns out, this choice of Binomial and Geometric
# can work, but not always. In the code below, 
# increase n to see what happens

p <- .25
n <- 100
x <- 0:(n)
mass.geom <- dgeom(x, p)
mass.bin <- dbinom(x, size = n, prob = p)

all_c <- choose(n,x) * (1-p)^(n - 2*x) * p^(x-1)
(c <- max(all_c))


plot(x, mass.geom, pch = 16, col = "red", type= "n")
points(mass.bin, pch = 16, col = "red", type= "h")
points(mass.geom, pch = 16, col = "blue", type = "h", lty = 2)


# Matching the means:
# choosing p* for rgeom so that np = (1-p*)/p*
p.star <- 1/(n*p + 1)
mass.geom <- dgeom(x, p.star)
mass.bin <- dbinom(x, size = n, prob = p)

all_c <- choose(n,x) * (1-p.star)^(n - 2*x) * p.star^(x-1)
(c <- max(all_c))


plot(mass.geom, pch = 16, col = "red", type= "n")
points(mass.bin, pch = 16, col = "red", type= "h")
points(mass.geom, pch = 16, col = "blue", type = "h", lty = 2)


## Answer 3 :

m <- 20
l <- 20
##for proposal we use the poisson(lambda) distribution for the target trunc poisson
##p_i/cq_i = I(i \in {0, 1, 2, ... , m})

##lets calculate c first
## do ??pois
c <- 1/(ppois(m, lambda = l))
c ## 1.013659 \Rightarrow higher acceptance rate

trunc_pois<- function(m = 20, n = 20)
{
  accept <- 0
  try <- 0
  while(!accept)
  {
    try <- try + 1
    prop <- rpois(1, lambda = l)
    
    if(prop <= m) accept <- 1
  }
  return(c(prop, try))
}
N <- 1e3
out <- replicate(N, trunc_pois())
mean(out[1, ])
mean(out[2, ])
c
hist(out[1, ])


## ans 4

x <- 0 : 50 ##trial values

all_c <- dgeom(x, p = 0.1)/dpois(x, lambda = 10)
max(all_c)
x <- 0:500
max(dgeom(x, prob = 0.1)/ dpois(x, lambda = 10))
