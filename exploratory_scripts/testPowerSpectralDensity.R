library(psd)

set.seed(0)
N <-32
n<- 0:(N-1)
w <- rnorm(1:N)
f1 <- 0.4
A1 <- 2
s <- A1*sin(2*pi*f1*n)
x <- s + w

plot(x~n)
