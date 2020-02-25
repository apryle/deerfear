dev.off()

t <- seq(0,200,by=0.1)  # time

# create x... made up of two underlying periodicities: 
#the first at a frequency of 1/16 or period of
#16 (one observation completes 1/16th of a full cycle, and a full cycle is completed every 16 observations)
#the second at a frequency of 1/5 (or period of 5). 
#The cosine coefficient (1.0) is larger than the sine coefficient (0.75).

x <- cos(2*pi*t/16) + 0.75*sin(2*pi*t/5) 
plot(x~t)

# basic spectrum
par(mfrow=c(2,1))
plot(t,x,'l')
spectrum(x)

# spectral density
par(mfrow=c(2,1))
plot(t,
     x,'l')
del<-0.1 # sampling interval
x.spec <- spectrum(x,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",
     type="l",
     xlim=c(0,1))


