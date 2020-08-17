# Functions to simulate and estimate transition matrices from ratios.

library(Matrix)

# function to simulate form Dirichlet distribution
rdirichlet<-function (n, alpha) 
  {
         l <- length(alpha)
         x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
         sm <- x %*% rep(1, l)
         x/as.vector(sm)
    }

# functio to estimate a transition matrix from a time series array
# the assumption is the array is in the format #timesteps x #states
# P_ij represents the probability moving from state i to state j.
estimateP <- function(ratio_ts){

  # the time series is in format # timesteps x #states
  T = dim(ratio_ts)[1]
  D = dim(ratio_ts)[2]

  # creating 1 step forward/backward arrays
  ts1 = ratio_ts[2:T,]
  ts0 = ratio_ts[1:T-1,]


  # vectorize the forward time series
  Y = as.vector(c(ts1))
  # create a block-diagonal matrix of the time series (repeated the number of states)
  X = as.matrix(bdiag(as.matrix(rep(list(ts0), D))))

  # solving the linear system
  p = qr.solve(X,Y) # p is (# states)^2
  # reshaping into a square transition matrix (# states x # states)
  P = matrix(p,nrow = D) 
  return(P)
}
  
# function to simulate ratios with a fixed transition matrix
simulateRatios <- function(P, ratio0, n_steps){
  ratio = ratio0
  ratios = ratio
  for (i in seq(1,n_steps)){
    ratio = ratio%*%P
    ratios = rbind(ratios, ratio)
  }
  return(ratios)
}







