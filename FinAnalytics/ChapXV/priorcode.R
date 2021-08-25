#Chapter XV
setwd(paste(homeuser,"/FinAnalytics/ChapXV",sep=""))

Npaths = 100
Nyears = 10
NdaysPerYr = 252
Ndays = NdaysPerYr*Nyears
muA = .07
muD = (1+muA)^(1/Ndays)-1    #daily ROR avg.
muD = exp(muA)^(1/Ndays)-1
sigmaA = .20
sigmaD = sigmaA/sqrt(Ndays) #daily volatility
rA = (muA - sigmaA^2/2)
rA
set.seed(2009)

#simulate:

sim <- function(init,Npaths,Ndays,muD,sigmaD,rD,isGBM) {
  X <- matrix(rep(0,Npaths*Ndays), nrow=Npaths, ncol=Ndays)
  X[,1] <- init #initial stock price
  for(t in 1:(Ndays-1)) {
    print(t)
    deltat = 1/NdaysPerYr
    tA = t/NdaysPerYr
    #Geomtric Brownian motion model:
    X[,t+1] <- X[,t]*exp( rA*deltat +
               sigmaA * sqrt(deltat) * rnorm(Npaths) )
  }
  return(X)
}

display <- function(X,Npaths,xlab,ylab) {
  #now go path by path:
  for(p in 1:Npaths)
    if(p==1) {
      plot(X[p,],col=p,type='l',ylim=c(50,100*8),
           xlab=xlab,ylab=ylab)
    } else {
            lines(X[p,],col=p)
    }
}

par(mfrow=c(1,1))
par(mar=c(2,2,1,1))
X <- sim(100.0,Npaths,Ndays,muD,sigmaD,rD,isGBM=TRUE)
display(X,Npaths,xlab="Days",ylab="Price")
min(X)
max(X)

setwd(paste(homeuser,"/FinAnalytics/ChapXV",sep=""))
taro<-read.csv("TARO.csv")
str(taro)

taro$Spread<-taro$Ask-taro$Bid
taro$Expiration<-as.Date(taro$Expiration)
taro$DataDate<-as.Date(taro$DataDate)
taro$Price<-(taro$Bid+taro$Ask)/2
taro$Maturity<-as.double(taro$Expiration-taro$DataDate)/365
taro$IV<-0.0
dates<-unique(taro$DataDate)
dates<-dates[1:150]
taro<-subset(taro,DataDate %in% dates)

bs<-function(type,S,K,sigma,t,r){
  d1 <- (log(S/K) + (r+(sigma^2)/2)*t) / (sigma*sqrt(t))
  d2 <- (log(S/K) + (r-(sigma^2)/2)*t) / (sigma*sqrt(t))
  if (type=='call') val <- pnorm(d1)*S - pnorm(d2)*K*exp(-r*t)
  else if (type=='put') val <- pnorm(-d2)*K*exp(-r*t) - pnorm(-d1)*S
  val
}

secantIV<-function(type,V,S,K,sigma0,sigma1,t,r){
  newSigma <- sigma0 - (bs(type,S,K,sigma0,t,r)-V)*(sigma0-sigma1)/
    (bs(type,S,K,sigma0,t,r) - bs(type,S,K,sigma1,t,r))
  if( abs(newSigma)==Inf ) return(0.0)
  if( abs(newSigma - sigma0) < .001 ) return(newSigma)
  else return(secantIV(type,V,S,K,newSigma,sigma0,t,r))
}

Val<-function(V,S,K,sigma,t,r){
  d1 <- (log(S/K) + (r+(sigma^2)/2)*t) / (sigma*sqrt(t))
  d2 <- (log(S/K) + (r-(sigma^2)/2)*t) / (sigma*sqrt(t))
  val<-pnorm(d1)*S - pnorm(d2)*K*exp(-r*t)
  return(val-V)
}

dVal<-function(V,S,K,sigma,t,r){
  d1 <- (log(S/K) + (r+(sigma^2)/2)*t) / (sigma*sqrt(t))
  val <- S*dnorm(d1)*sqrt(t)
  return(val)
}

impliedVol<-function(V,S,K,sigma,t,r){
  newSigma <- sigma - Val(V,S,K,sigma,t,r) / dVal(V,S,K,sigma,t,r)
  if( abs(newSigma - sigma) < .001 ) return(newSigma)
  else return(impliedVol(V,S,K,newSigma,t,r))
 }

impliedVol(2.875,24,22,0.2,.5,0.05)
secantIV('call',2.875,24,22,0.5,1,.5,0.05)

for(date in dates){
  sub<-subset(taro,DataDate==date)
  IV<-rep(0,dim(sub)[1])
  for(i in 1:dim(sub)[1]){
    IV[i]<-secantIV(sub$Type[i],sub$Price[i],sub$UnderlyingPrice[i],
                    sub$Strike[i],0.4,1,sub$Maturity[i],0.05)
  }
  taro[taro$DataDate==date,]$IV<-IV
}
taro<-subset(taro,IV!=0.0)
hist(taro$IV,breaks=100,main="")

hist(taro$Spread,main="")
vol<-data.frame(date=dates)
vol$IV<-0.0
for(date in dates){
 vol[vol$date==date,]$IV<-mean(taro[taro$DataDate==date,]$IV)
}
plot(vol$date,vol$IV,type='l',col='blue')

library(ggplot2)
ggplot(vol,aes(x=date,y=IV)) + geom_line()

tarosub<-taro[taro$DataDate=='2002-03-25',]
taroput<-subset(tarosub,Type=='put' & UnderlyingPrice > Strike)
tarocall<-subset(tarosub,Type=='call' & UnderlyingPrice < Strike)

x<-append(taroput[1:3,]$Strike,tarocall[1:6,]$Strike)
y<-append(taroput[1:3,]$IV,tarocall[1:6,]$IV)
plot(x,y,type='l', xlab='Strike', ylab='Implied Volatility')

#Rcpp package:
library(Rcpp)
sourceCpp(paste(homeuser,"/FinAnalytics/ChapXV/fibonacci.cpp",sep=""))
fibonacci(20)

sourceCpp(paste(homeuser,"/FinAnalytics/ChapXV/secant.cpp",sep=""))

CsecantIV<-function(type,V,S,K,sigma0,sigma1,t,r){
  if(type=='call') val<-secant(0,V,S,K,sigma0,sigma1,t,r)
  else if(type=='put') val<-secant(1,V,S,K,sigma0,sigma1,t,r)
  val
}

secantIV('call',2.875,24,22,0.5,1,.1,0.05)
CsecantIV('call',2.875,24,22,0.5,1,.1,0.05)
## additional unit test:
secantIV('put',2.875,24,22,0.5,1,.1,0.05)
CsecantIV('put',2.875,24,22,0.5,1,.1,0.05)
## 
dates<-dates[1:3]
dates
sub<-subset(taro,DataDate %in% dates)
n<-dim(sub)[1]
system.time(for(i in 1:n) sub$IV[i]
      <- secantIV(sub$Type[i],
           sub$Price[i],
           sub$UnderlyingPrice[i],
           sub$Strike[i],
           0.4,
           1,
           sub$Maturity[i],
           0.05)
)
system.time(for(i in 1:n) sub$IV[i]
      <- CsecantIV(sub$Type[i],
          sub$Price[i],
          sub$UnderlyingPrice[i],
          sub$Strike[i],
          0.4,
          1,
          sub$Maturity[i],
          0.05)
)
