### Paste all your codes for model building
### and cross-validation here
gamma_ick <- function(x, mu, sig2, pis, C)
{
  gamma_prob <- matrix(0, nrow = length(x), ncol = C)
  for(c in 1:C)
  {
    gamma_prob[ ,c]  <- dnorm(x, mean = mu[c], sd = sqrt(sig2[c]))* pis[c]
  }
  
  gamma_prob <- gamma_prob/(rowSums(gamma_prob))
  return(gamma_prob)
}

GMMoneDim <- function(x, C )
{
  pis <- numeric(length = C)
  for(i in 1:C){
    pis[i] <- runif(1)
  }
  pis <- pis/sum(pis)
  mu <- seq(min(x), max(x), length.out = C)
  sig2 <- rep(var(x), C)
  diff <- 100
  tol <- 1e-5
  iter <- 0
  
  current <- c(pis, mu, sig2)
  store <- current
  
  while(diff > tol)
  {
    previous <- current
    iter <- iter + 1
    Ep <- gamma_ick(x, mu, sig2, pis, C)
    
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
  
  print(current)# final estimates
  
  
  # Final estimates of the probability
  # that each observation is in Class C.
  Prob.Z <- gamma_ick(x, mu = mu, sig2 = sig2, pis = pis, C)
  Prob.Z <- apply(Prob.Z, 1, which.max)
  rtn <- list(Prob.Z, mu, sig2, pis)
  return(rtn)
}

dat <- read.csv("assign4_train.csv")
y <- dat$y
erup2 <- GMMoneDim(y, C = 2)
erup3 <- GMMoneDim(y, C = 3)

log.like <- function(y, mu, sig2, pis) {
  n <- length(y)
  logL <- 0
  for (i in 1:n) {
    logL <- logL + log(sum(pis * dnorm(y[i], mu, sqrt(sig2))))
  }
  return(logL)
}

n <- length(y)
loglike2 <- log.like(y, mu = erup2[[2]], sig2 = erup2[[3]], pis = erup2[[4]])
loglike3 <- log.like(y, mu = erup3[[2]], sig2 = erup3[[3]], pis = erup3[[4]])
# number of parameters
C <- 2
K2 <- 3*C - 1
C <- 3
K3 <- 3*C - 1
BIC.2 <- -2*loglike2 + K2*log(n)
BIC.3 <- -2*loglike3 + K3*log(n)

BIC_scores <- c(BIC.2,BIC.3)
iter <- length(BIC_scores)
for(i in 1:length(BIC_scores)){
  if(BIC_scores[i]<BIC_scores[iter])
    iter = i
}

cluster_classif <- list(erup2[[1]], erup3[[1]])
optimal_cluster <- data.frame(cluster_classif[[iter]])

n <- unique(optimal_cluster$cluster_classif..iter..)[1]
mat <- c()
vec1 <- which(optimal_cluster==1)
vec2 <- which(optimal_cluster==2)

cluster1 <- dat[c(vec1),]
cluster2 <- dat[c(vec2),]


y1 <- cluster1$y
x1 <- data.matrix(cluster1[,-c(1)])
y2 <- cluster2$y
x2 <- data.matrix(cluster2[,-c(1)])

fn <- function(x,y){
  ls <- seq(1e-5,1,by=1e-5)
  betas <- matrix(0,length(ls), 100)
  err <- numeric(length= length(ls))
  for(i in 1:length(ls)){
    betas[i,]<- solve(t(x) %*% x + diag(ls[i], 100)) %*% t(x) %*% y
    y_pred <- x %*% betas[i,]
    err[i]<-norm(y-y_pred, "2")
  }
  #err
  fin_err= min(err)
  index= which.min(err)
  return(betas[index,])
}

beta1 <- fn(x1,y1)
beta2 <- fn(x2,y2)

fin_beta <- (dim(x1)[1]*beta1 + dim(x2)[1]*beta2)/(dim(x1)[1]+dim(x2)[1])
obj <- fin_beta
save(obj, file = "fit_params.Rdata")