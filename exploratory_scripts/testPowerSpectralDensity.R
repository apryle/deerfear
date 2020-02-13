library(psd)

set.seed(0)

# create a toy time variable
N <-32 # N is a number. Equivalent to the number samples from one axis (101)
n<- 0:(N-1) # n is a vector representing time. Equivalet to my datetimemil

# create a toy dataset with a "hidden" wave form
w <- rnorm(1:N) 
f1 <- 0.4
A1 <- 2
s <- A1*sin(2*pi*f1*n) 
x <- s + w  # x is equivalent to my acc value on one axis

plot(x~n, type="l")

xPer <- (1/N)*abs(fft(x)^2) # power 
f    <- seq(0, 1.0-1/N, by=1/N) # frequency

plot(xPer~f, type="l")
