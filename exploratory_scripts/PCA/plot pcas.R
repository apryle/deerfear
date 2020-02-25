
str(pca.obj$rotation)
pcarot <- as.data.frame(pca.obj$rotation)
str(pcarot)
head(pcarot)


#################
# split out by axis and pc

plotpcas

# create the breaks for each axis
adder <- nrow(pcarot) / 3
xrng <- c(1:adder)
yrng <- c((adder+1):(adder*2))
zrng <- c((adder*2+1):(adder*3))
myrng <- cbind(xrng, yrng, zrng)

axis <- c("x", "y", "z")

numPCAs <- 2

# set up plotting space
par(mfrow=c(numPCAs,3))

for(p in 1:numPCAs){
  j <- 1
  for(j in 1:3){
    print(paste0("PC", p, " Axis", j))
    rng <- myrng[ ,j]
    print(rng)
    print(length(rng))
    mypca <- pcarot[rng, p]
    print(tail(mypca))
    plot(mypca, type="l", main=paste0("PC", p, " Axis ", j))
  }
}
  
