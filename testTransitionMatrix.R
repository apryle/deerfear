source("transitionMatrix.R")

# random ratios

T = 1000
D = 4

# test on random dirichlet observations
ratio_timeseries = rdirichlet(T, alpha = replicate(D,1))
# the time series is in format # timesteps x #states
dim(ratio_timeseries)
# estimate P
P = estimateP(ratio_timeseries)
print(P)


ratio_timeseries = rdirichlet(T, alpha = c (1,1,5,5))
P = estimateP(ratio_timeseries)
print(P)
colSums(P)
rowSums(P)

# test with generating ratio with a known transition matrix
initialRatio = rdirichlet(1, alpha = c (1,1,1,1))
ts = simulateRatios(P, initialRatio, T)
P_estimated = estimateP(ts)
print(P - P_estimated)
