################################################################################
# Part 2 - Check density of difference of random uniform variates on circle
x<-runif(10000,0,2*pi)
y<-runif(10000,0,2*pi)
par(mfrow=c(1,3)) 
dens(abs(x - y))
dens(pmin((2 * pi) - abs(x - y), abs(x - y)))
plot(abs(x-y)~pmin((2 * pi) - abs(x - y), abs(x - y)))
