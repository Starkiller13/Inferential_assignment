set.seed(29)
n = c(10,15,10,15) #Number of elements for every col
var = c(1,1,1,1) #Variance for every col
mu = c(0,0,0,0) #Mean for every col
k=4 #Number of cols
N = 1e6#Number of iteration for simulation
sim.N.F <- rep(0,N)
for(l in 1:N){
    y <- rnorm(sum(n), mean = 0, sd = sqrt(var[1]))#Same variance for every Yij
    estMuj <- c(sum(y[1:10])/n[1], 
            sum(y[11:25])/n[2],
            sum(y[26:35])/n[3], 
            sum(y[36:50])/n[4])
    estMu <- sum(y)/sum(n)
    SSR <- 0
    SSE <- 0
    for (i in 1:k){
        offset <- sum(n[1:(i-1)])
        for(j in 1:n[i]){
            SSR = SSR + (estMuj[i]-estMu)^2
            SSE = SSE + (y[offset+j]-estMuj[i])^2
        }
    }
    sim.N.F[l] <- (SSR/(k-1))/(SSE/(sum(n)-k)) #value of F observed (Fobs)
}
hist(sim.N.F, freq=FALSE, xlab="F",breaks = seq(min(sim.N.F), max(sim.N.F), length.out = 50), ylab="Distribution")
plot(function(x) df(x, df1=k-1, df2=sum(n)-k),xlim=c(0,8), col='red', add=TRUE)
legend(3.5, 0.4, legend=c("Exact distribution"),
       col=c("red"), lwd=1,lty=c(1), pch=c(NA), cex=0.8)
