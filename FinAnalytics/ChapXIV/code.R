#Chapter XIV
#Binomial option pricing adapted from E.G.Haug book.
r = .08
b = r
sigma = .30
S = 100
K = 95
T = .5
binomial <- function(type,S,K,sigma,t,r,n) {
  deltat = T/n
  u = exp(sigma*sqrt(deltat))
  d = exp(-sigma*sqrt(deltat))
  p = (exp(b*deltat)-d)/(u-d)
  a = ceiling(log(K/(S*d^n))/log(u/d))
  val = 0
  if(type=='call') {
    for(i in a:n) {
      val = val +
        (factorial(n)/(factorial(i)*factorial(n-i)))*
        p^i*(1-p)^(n-i)*(S*u^i*d^(n-i)-K)
    }
  } else if(type=='put') {
    for(i in 0:(a-1))
      val = val +
      (factorial(n)/(factorial(i)*factorial(n-i)))*
      p^i*(1-p)^(n-i)*(K-S*u^i*d^(n-i))
  }
  exp(-r*T)*val
}

bs<-function(type,S,K,sigma,t,r){
  d1 <- (log(S/K) + (r+(sigma^2)/2)*t) / (sigma*sqrt(t))
  d2 <- (log(S/K) + (r-(sigma^2)/2)*t) / (sigma*sqrt(t))
  if (type=='call') val <- pnorm(d1)*S - pnorm(d2)*K*exp(-r*t)
  else if (type=='put') val <- pnorm(-d2)*K*exp(-r*t) -
    pnorm(-d1)*S
  val
}

#Invoke Binomial Method varying n:
N = 64
par(mfrow=c(1,2))
bmCallVal <- rep(0,length(1:N))
for(n in 1:N)
  bmCallVal[n] <- binomial('call',S,K,sigma,T,r,n)
plot(bmCallVal)
lines(bmCallVal,col=4)
bsCallVal <- bs('call',S,K,sigma,T,r)
lines(rep(bsCallVal,N),col=4)
bmPutVal <- rep(0,length(1:N))
for(n in 1:N)
  bmPutVal[n] <- binomial('put',S,K,sigma,T,r,n)
plot(bmPutVal)
lines(bmPutVal,col=4)
bsPutVal <- bs('put',S,K,sigma,T,r)
lines(rep(bsPutVal,N),col=4)

bmCallVal[N]
bsCallVal
bmCallVal[N]/bsCallVal
bmPutVal[N]
bsPutVal
bmPutVal[N]/bsPutVal

#Visualizaing Put-Call Parity:
S <- 75:125
M = length(S)
bmCallVal <- vector(length=M)
bmPutVal <- vector(length=M)
n = 64
for(i in 1:M) {
  bmCallVal[i] <- binomial('call',S[i],K,sigma,T,r,n)
  bmPutVal[i] <- binomial('put',S[i],K,sigma,T,r,n)
}
par(mfrow=c(1,1))
plot(S,bmCallVal,type='l',col=4,
     ylab="bmCallVal,bmPutVal")
lines(S,bmPutVal,col=5)
#At the present value of the strike, K*exp(-r*T),
#the call and put have the same value (ATM).
pvK <- K*exp(-r*T)
abline(v = pvK)
text(c(pvK),c(30),paste("K*exp(-r*T) =",
                        round(pvK,2)),cex=.75)

#Let's check Put-Call Parity:
#S=100
l = ceiling(M/2) #Find the middle price S[l]
S[l]
round(bmCallVal[l],4)
round(bmCallVal[l],4) == round(bmPutVal[l] + S[l] - pvK,4)
