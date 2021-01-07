tau <- function(theta){
    out <- theta/(1-theta)
    out2 <- log(out)
    return(out2)
}
set.seed(29)
N = 1e5
theta0 =2/3
sim.N.nb <-matrix(NA, nrow = N, ncol = 2)
# put the obs.intervals here> 
for(i in 1:N){
    xnegbin10 <- rnbinom(n=10,size=10,prob=theta0)
    xnegbin50 <- rnbinom(n=50,size=10,prob=theta0)
    sim.N.nb[i,1] <- tau(100/(100+sum(xnegbin10)))
    sim.N.nb[i,2] <- tau(500/(500+sum(xnegbin50)))
}
mean10 = mean(sim.N.nb[,1])
mean50 = mean(sim.N.nb[,2])
var10 = var(sim.N.nb[,1])
print(var10)
var50 = var(sim.N.nb[,2]) 
true_mean = (tau(2/3))
true_var10 <- (1/(theta0*(1-theta0)))^2
true_var10 <- true_var10*(theta0^2*(1-theta0))/100
true_var50 <- (1/(theta0*(1-theta0)))^2
true_var50 <- true_var50*(theta0^2*(1-theta0))/500
hist(sim.N.nb[,1], freq = FALSE, breaks=20, xlab = "n = 10 extimated p.d.f")
plot(function(x) dnorm(x, mean = true_mean, sd=sqrt(true_var10)),xlim=c(0,2),add=TRUE,ylim=8)
hist(sim.N.nb[,2], freq = FALSE, breaks=20, xlab = "n = 50 extimated p.d.f")
plot(function(x) dnorm(x, mean = true_mean, sd=sqrt(true_var50)),xlim=c(0,2),add=TRUE,ylim=8)