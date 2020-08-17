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


# try using with a deer dataset
states = read.csv("~/Downloads/5minContinuous-v2.csv")

simple_states  = states

# making a category Foraging and Traveling
simple_states$Duration.of.Foraging.and.Traveling = 0
simple_states$Duration.of.Foraging.and.Traveling[states$Duration.of.Foraging>0 & states$Duration.of.travel>0] = pmin(states$Duration.of.travel, states$Duration.of.Foraging)[states$Duration.of.Foraging>0 & states$Duration.of.travel>0]
simple_states$Duration.of.Foraging[states$Duration.of.Foraging>0 & states$Duration.of.travel>0] = (simple_states$Duration.of.Foraging - simple_states$Duration.of.Foraging.and.Traveling)[states$Duration.of.Foraging>0 & states$Duration.of.travel>0]
simple_states$Duration.of.travel[states$Duration.of.Foraging>0 & states$Duration.of.travel>0] = 0

# making bedded and vigilance to bedded
simple_states$Duration.of.Vigilance[states$Duration.Bedded>0 & states$Duration.of.Vigilance>0 ] = 0
simple_states$Duration.Bedded[states$Duration.Bedded>0 & states$Duration.of.Vigilance>0 ] = pmax(states$Duration.Bedded, states$Duration.of.Vigilance)[states$Duration.Bedded>0 & states$Duration.of.Vigilance>0]
# simple_states$Duration.of.Foraging[states$Duration.of.Foraging>0 & states$Duration.of.travel>0] = 0

# making bedding and foraging to bedded
# simple_states$Duration.of.Foraging[states$Duration.Bedded>0 & states$Duration.of.Foraging>0 ] = 0

# getting only the interesting states and normalizing
simple_states = simple_states[,c(6:9,12)]/rowSums(simple_states[,c(6:9,12)])

# estimating the transition matrix
P = estimateP(as.matrix(simple_states))
colnames(P) = colnames(simple_states)
rownames(P) = colnames(simple_states)

# installed.packages("diagram")
# library(diagram)
# library(expm)
# library(diagram)
# library(pracma)
# plotmat(P,pos = c(1,2),
#        lwd = 1, box.lwd = 2,
#        cex.txt = 0.8,
#        box.size = 0.1,
#        box.type = "circle",
#        box.prop = 0.5,
#        box.col = "light yellow",
#        arr.length=.1,
#        arr.width=.1,
#        self.cex = .4,
#        self.shifty = -.01,
#        self.shiftx = .13,
#        main = "")

library(markovchain)
devtools::install_github('spedygiorgio/markovchain')


# making rows sum to 1

P_tm = P
P_tm[P_tm<0]=0
P_tm[,5] = 1 - rowSums(P_tm[,1:4])

mcA<-new("markovchain", transitionMatrix=P_tm)
plot(mcA)

