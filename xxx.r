set.seed(29)
n = c(10,15,10,15)
var = c(1,1,1,1)
mu = c(0,0,0,0)
k=4
y13 <- matrix(NA,nrow=n[1],ncol=2) 
y24 <- matrix(NA,nrow=n[2],ncol=2) 
for (i in 1:k){
    if(i%%2==0){
        y24[,i%/%2] <- rnorm(n[i], mean = mu[i], sd = sqrt(var[i]))
    }else{
        y13[,i%/%2+1] <- rnorm(n[i], mean = mu[i], sd = sqrt(var[i]))
    }
}
print(y13)
print(y24)

estMuj <- c(sum(y13[,1])/n[1], sum(y24[,1])/n[2], sum(y13[,2])/n[3], sum(y24[,2])/n[4])

estMu <- (sum(y13[,1])+sum(y13[,2])+sum(y24[,1])+sum(y24[,2]))/sum(n)


SSR <- 0
SSE <- 0
for (i in 1:k){
    if(i%%2==0){
        for(j in 1:n[i]){
            SSR = SSR + (estMuj[i]-estMu)^2
            SSE = SSE + (y24[j,i%/%2]-estMuj[i])^2
        }
    }else{
        for(j in 1:n[i]){
            SSR = SSR + (estMuj[i]-estMu)^2 
            SSE = SSE + (y13[j,i%/%2+1]-estMuj[i])^2
        }
    }
}

print(SSR)
print(SSE)
valueF = (SSR/(k-1))/(SSE/(sum(n)-k))
print(valueF)
p_value <- pf(valueF,k-1,sum(n)-k)
plot(function(x) df(x, df1=k-1,df2=sum(n)-k),xlab="p",ylab="density", col='red')
print(p_value)