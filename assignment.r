tau <- function(theta){
    out <- theta/(1-theta)
    out2 <- log(out)
    return(out2)
}

inverse_tau <- function(t) {
    o <- exp(t)
    o1 <- o/(exp(t)+1)
    return(o1)
} 
set.seed(29)
N = 1e5
sim.N.nb <-matrix(NA, nrow = N, ncol = 2)
# put the obs.intervals here> 
for(i in 1:N){
    xnegbin10 <- rnbinom(n=10,size=10,prob=2/3)
    xnegbin50 <- rnbinom(n=50,size=10,prob=2/3)
    sim.N.nb[i,1] <- tau(100/(100+sum(xnegbin10)))
    sim.N.nb[i,2] <- tau(500/(500+sum(xnegbin50)))
}
mean10 = mean(sim.N.nb[,1])
mean50 = mean(sim.N.nb[,2])
var10 = var(sim.N.nb[,1])
print(var10)
var50 = var(sim.N.nb[,2]) 
true_mean = (tau(2/3))
true_var10 = 200*(1/3)/(2/3)^2*(1/((2/3)*(1/3)))^2/1e5
true_var50 = 40*(1/3)/(2/3)^2*(1/((2/3)*(1/3)))^2/1e5
par(mfrow =c(1,2))
hist(sim.N.nb[,1], freq = FALSE, breaks=20)
plot(function(x) dnorm(x, mean = true_mean, sd=sqrt(0.1111)),xlim=c(0,2),add=TRUE)
hist(sim.N.nb[,2], freq = FALSE, breaks=20)
plot(function(x) dnorm(x, mean = true_mean, sd=sqrt(0.1111*)),xlim=c(0,2),add=TRUE)