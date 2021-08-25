#Chapter VIII
mu_d = .05
mu_e = .12
sigma_e = .30
sigma_d = .20
sigma_de = .003
w_d = seq(0,1,.01)
mu_P = vector(length=length(w_d))
sigma_P = vector(length=length(w_d))
sr_P = vector(length=length(w_d))
for(u in 1:length(w_d)) {
  mu_P[u] = mu_d*w_d[u] + mu_e*(1-w_d[u])
  sigma_P[u] = sqrt(w_d[u]^2*sigma_d^2 +
                      (1 - w_d[u])^2*sigma_e^2 +
                      2*w_d[u]*(1 - w_d[u])*sigma_de)
  sr_P[u] = mu_P[u] / sigma_P[u]
}
par(mfrow=c(1,2))
plot(sigma_P,w_d,type="l",ylab="w_d",col=6)
ind_min_var_P = sigma_P == min(sigma_P)
w_d[ind_min_var_P]
points(sigma_P[ind_min_var_P],w_d[ind_min_var_P])
text(sigma_P[ind_min_var_P]+.04,w_d[ind_min_var_P],
     paste("<-(",round(sigma_P[ind_min_var_P],4),",",
           w_d[ind_min_var_P],")"),cex=.75)
#Now plot sigma_P as a function of mu_P
plot(sigma_P,mu_P,type="l",ylab="mu_P",col=2)
mu_P[ind_min_var_P]
points(sigma_P[ind_min_var_P],mu_P[ind_min_var_P])
text(sigma_P[ind_min_var_P]+.045,mu_P[ind_min_var_P],
     paste("<-(",round(sigma_P[ind_min_var_P],4),",",
           mu_P[ind_min_var_P],")"),cex=.75)
ind_opt_P = sr_P == max(sr_P)
mu_P[ind_opt_P]
points(sigma_P[ind_opt_P],mu_P[ind_opt_P])
text(sigma_P[ind_opt_P]+.045,mu_P[ind_opt_P],
     paste("<-(",round(sigma_P[ind_opt_P],4),
           ",",mu_P[ind_opt_P],")"),cex=.75)

library(quadprog)
library(tseries)
P = 2*diag(c(1,2,4))
d = c(1,1,-5)
At = matrix(0,nrow=3,ncol=3)
At[1,] = c(-1,0,-1)
At[2,] = c(1,0,0)
At[3,] = c(0,-1,0)
b0 = c(-1,5,0)
P
d
At
b0
xHat = solve.QP(P, d, t(At), b0)$solution
xHat

A = matrix(c(2,0,1,-1,
             0,2,1,1,
             1,1,0,0,
             -1,1,0,0),nrow=4,ncol=4)
b = c(1,4,1,1)
u = solve(A) %*% b
u

library(huge)
data(stockdata)
len = length(stockdata$data[,1])
D = dim(stockdata$data)[2]
prices = stockdata$data[,1:D]
lab = stockdata$info[1:D,1]
isSplitAdjusted=FALSE
daysPerYr=252; mufree = 0
R <- findR(prices,
     isSplitAdjusted=isSplitAdjusted) #side affects prices
displayCharts(prices,lab,nrow=6,ncol=4,sleepSecs=5)
dim(R)
res   <- findCovMat(R)
meanv <- res[[1]]
cov_mat <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]
round(cov_mat[1:8,1:8],4)
Sharpe <- (meanv-mufree)/sdevv
isSplitAdjusted <- TRUE
isPlot <- TRUE

isShorting <- FALSE
Amat <- cbind(rep(1,D),meanv,diag(1,nrow=D)) #no short sales
Amat[1:8,1:10]
findWeights <- function(muP,cov_mat,Amat) {
  bvec = c(1,muP,rep(0,D)) #no short sales
  D <- dim(cov_mat)[1]
  result = solve.QP(Dmat=2*cov_mat,dvec=rep(0,D),
                    Amat=Amat,bvec=bvec,meq=2)
  result
}
par(mfrow=c(4,3))
maxMeanV <- max(meanv)
plot(sort(meanv),col=4)
abline(h=maxMeanV,col=2)
text(D/2,maxMeanV,round(maxMeanV,4),col=4)
maxMeanV
for(muP in c(maxMeanV,.27,.24,.21,
             .18,.15,.12,.09,.06,.03)) {
  result <- findWeights(muP,cov_mat,Amat)
  if(length(result[[1]])>0 && !is.na(result[[1]][1])) {
    summary(result)
    w = result$solution
    sum(w)
    round(w,4)
    plot(1:length(w),w,cex=.01,
         xlab=paste("muP =",round(muP,4)))
    text(1:length(w),w,lab,col=4,cex=.75)
  } else {
    stop("NA result")
  }
}
lab[w > 0.00001]
round(w[w > 0.00001],4)
t(w) %*% meanv
library(quadprog)
opt <- function(lab,meanv,cov_mat,isShorting,Nruns=100) {
  if(isShorting) {
    #set the constraints matrix
    Amat = cbind(rep(1,D),meanv)
  } else {
    Amat = cbind(rep(1,D),meanv,diag(1,nrow=D)) #no short sales
  }
  if(isShorting) {#set of Nruns possible target values
    #for expect portfolio return
    muP = seq(.05,.60,length=Nruns)
  } else {
    muP = seq(min(meanv)+.0001,max(meanv)-.0001,
              length=Nruns) #no short sales
  }
  muP
  sdP = muP # set up storage for sdev of port rets
  weights = matrix(0,nrow=Nruns,ncol=D) #store port weights
  W <- 4
  u <- 1/2
  # find the optimal portfolios for each target expected return
  for (i in 1:length(muP))
  {
    if(isShorting) {
      bvec = c(1,muP[i]) # constraint vector
    } else {
      bvec = c(1,muP[i],rep(0,D)) #no short sales
    }
    #print(paste(2*cov_mat,rep(0,D),Amat,bvec))
    isPlot = TRUE
    result = solve.QP(Dmat=2*cov_mat,dvec=rep(0,D),
                      Amat=Amat,bvec=bvec,meq=2)
    sdP[i] = sqrt(result$value)
    #weights are contained in result solution
    weights[i,] = result$solution
    mufree = 1.3/daysPerYr # input value of risk-free int rate
    sharpe =(muP-mufree)/sdP # compute Sharpe Ratios
    ind = (sharpe == max(sharpe)) # Find maximum Sharpe Ratio

    if(isPlot && (i%%10)==0) {
      print(i)
      par(mar=c(3.82,2.82,2.82,0.82))
      par(mfrow=c(ceiling((min(10,D+3))/W),W)) #3 extra plots
      for(d in 1:min(49,D)) {
        plot(round(weights[,d],3),xlab=lab[d])
      }
      plot(weights[i,],xlab=paste("weights,i =",i))
      plot(sharpe[1:i],xlab="sharpe",xlim=c(1,Nruns))
      plot(muP[1:i],xlab="mu",xlim=c(1,Nruns))
      Sys.sleep(5*u)
    }
  }
  Sys.sleep(15*u)
  round(weights[ind,],6)
  for (i in 1:length(muP))
    w = vector(length=D)
  w[] = 0
  for(d in (1:D)){
    weight = round(weights[ind,d],3)
    if(weight > .001)
      w[d] = weight
    print(paste(lab[d],weight*100,"%"))
  }
  for(i in 1:Nruns) if(ind[i]) print(i)
  return(w)
}

#huge case
res <- elimSyms(prices,lab,dir,isSubDir=FALSE)
prices <- res[[1]]
lab    <- res[[2]]
R <- findR(prices)
D <- dim(prices)[2]
res <- findCovMat(R)
meanv    <- res[[1]]
cov_mat  <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]
checkCovMat(cov_mat)
mufree <- 0
res    <- pruneBySharpe(prices,lab,meanv,sdevv,.075)
prices <- res[[1]]
lab    <- res[[2]]
R   <- findR(prices)
res <- findCovMat(R)
meanv    <- res[[1]]
cov_mat  <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]
sdevv <- isnaCheckCovMat(R)
checkDeterminant(prices,R,lab)
isShorting <- FALSE
daysPerYr <- 252
library(quadprog)
w <-opt(lab,meanv,cov_mat,isShorting)
t(cbind(lab[w > 0],w[w > 0]))
maxw = max(w+.02)
plot(w,ylim=c(0.01,maxw))
text(w,lab,cex=.65,pos=3,col=4)

weightPortOOS <- function(lab,len,D,w,prices=NA,
                          start="2013-11-29",end="2014-11-28",
                          startBck1="2013-11-28",startFwd1="2013-11-27",
                          isNaive=FALSE,cached=NA) {#len x D prices
  if(length(prices) == 1 && is.na(prices)) {
    obtainedPrices = getHistPrices(lab,w,len,start=start,end=end,
                  startBck1=startBck1,startFwd1=startFwd1,cached=cached)
    existLen = dim(obtainedPrices)[1]
    prices = as.matrix(obtainedPrices[(existLen-len+1):existLen,])
  }
  numNonZeroWs = sum(ceiling(w))
  portv = as.vector(rep(0,len))
  D = length(w)
  for(i in 1:len) {
    for(d in 1:D) { #roll down a return line
      if(w[d] > 0)
        if(!isNaive) {
          portv[i] = portv[i] +
        w[d]*prices[i,d]/prices[1,d]
      } else {
        portv[i] <- portv[i] +
        (1/numNonZeroWs)*prices[i,d]/prices[1,d]
      }
    }
  }
  return(portv)
}
#unit test:
weightPortOOS(c('^GSPC'),251,1,c(1.0))
weightPortOOS(c('BKNG'),251,1,c(1.0))
weightPortOOS(c('^GSPC','BKNG'),251,2,c(.1,.9))

#Stocks: six years 2008 through 2014:
dir   <- "MVO6"
start <- "2008-02-14"
end   <- "2014-02-14"
isPlotInAdjCloses <- FALSE
isCacheEnabled <- TRUE
createDirs(dir)
res <- readSubDirs(dir)
D1  <- res[[1]]
D2  <- res[[2]]
lab <- res[[3]]
len <- 1512
D <- D1 + D2
prices <- matrix(rep(NA,len*D),nrow=len,ncol=D)
library(tseries)
prices <- acquirePrices(prices,lab,len,D,D1,D2,dir,
                        start=start,end=end,isSubDir=TRUE)
savePrices <- prices
saveLab <- lab

res <- elimSyms(prices,lab,dir,isSubDir=TRUE)
prices <- res[[1]]
lab    <- res[[2]]
R <- findR(prices)
D <- dim(prices)[2]
res <- findCovMat(R)
meanv    <- res[[1]]
cov_mat  <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]
checkCovMat(cov_mat)
mufree <- 0
res    <- pruneBySharpe(prices,lab,meanv,sdevv,.0400)#.0456)
prices <- res[[1]]
lab    <- res[[2]]
R   <- findR(prices)
res <- findCovMat(R)
meanv    <- res[[1]]
cov_mat  <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]

sdevv <- isnaCheckCovMat(R)
checkDeterminant(prices,R,lab)
isShorting <- FALSE
daysPerYr <- 252
library(quadprog)
w <-opt(lab,meanv,cov_mat,isShorting)
par(mfrow=c(1,1))
maxw = max(w+.02)
plot(w,ylim=c(0.01,maxw))
text(w,lab,cex=.55,pos=3,col=4)
t(cbind(lab[w > 0],w[w > 0]))
#Write out w and lab results to CSV file
writeWeights <- function() {
  numNonZeroWs = sum(ceiling(w))
  QPtype <- 1
  setwd(paste(homeuser,"/FinAnalytics/",dir,"/",sep=""))
  fileName = paste("resD",numNonZeroWs,"QP",toString(QPtype),
                   "Days",len,".csv",sep="")
  if(file.exists(fileName))
    stop(paste(getwd(),fileName,"already exists"))
  contents = cbind(lab,w)
  o <- order(-w)
  write.csv(contents[o,][1:numNonZeroWs,],file=fileName)
}
writeWeights()
t(cbind(lab[w > 0],w[w > 0]))

#ETFs:
dir   <- "ETF"
start <- "2012-05-02"
end   <- "2015-05-01"
len <- length(get.hist.quote("QQQ",quote="Adj",start=start,end=end))
daysPerYr = 252
isPlotInAdjCloses <- FALSE
isCacheEnabled    <- TRUE
createDirs(dir,isSubDir=FALSE)
res <- readSubDirs(dir,isSubDir=FALSE)
D   <- res[[1]]
lab <- res[[2]]
prices <- matrix(rep(NA,len*D),nrow=len,ncol=D)
library(tseries)
prices <- acquirePrices(prices,lab,len,D,D1,D2,
                        start=start,end=end,dir,isSubDir=FALSE)
sum(is.na(prices[1,]))
price1v <- ifelse(is.na(prices[1,]),-1000,prices[1,])
plot(price1v,col=4)

res <- elimSyms(prices,lab,dir,isSubDir=FALSE)
prices <- res[[1]]
lab    <- res[[2]]
sum(is.na(prices[1,]))==0 #assert there are no NA prices in first row
isSplitAdjusted <- TRUE
R <- findR(prices)
res <- findCovMat(R)
meanv    <- res[[1]]
cov_mat  <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]
checkCovMat(cov_mat)
mufree <- 0
res    <- pruneBySharpe(prices,lab,meanv,sdevv,.0827)#.085)
prices <- res[[1]]
lab    <- res[[2]]
sum(is.na(prices[1,]))
R   <- findR(prices)
res <- findCovMat(R)
meanv    <- res[[1]]
cov_mat  <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]
R <- findR(prices)
sdevv <- isnaCheckCovMat(R)
checkDeterminant(prices,R,lab,isSubDir=FALSE)
isShorting <- FALSE

library(quadprog)
w <- opt(lab,meanv,cov_mat,isShorting)
portv <- weightPortOOS(lab,len,D,w,   prices=prices,#arg added 2017/05/21
                       start=start,end=end,cached=c("BSJF"))
sp <- weightPortOOS(c('^GSPC'),len=len,1,c(1.0),
                    prices=NA,start=start,end=end)
par(mfrow=c(1,1))
plot(meanv,col=4,cex=0)
text(meanv,lab,cex=1,col=4)

plot(portv,type="l",ylim=c(.5,1.9),
     main="",xlab="days")
lines(sp,type="l",col="green")
par(mfrow=c(1,1))
maxw = max(w+.025)
plot(w,ylim=c(0.01,maxw),col=4)
text(w,lab,cex=1,pos=3,col=4)
writeWeights("ETF",lab,w)
t(cbind(lab[w > 0],w[w > 0]))

displayCharts(prices,lab,nrow=3,ncol=4,sleepSecs=2)
interestingIdxs <- sort(c(match('PJP',lab),
                     match('VHT',lab),
                     match('BOND',lab),
                     match('VTV',lab),
                     match('SDY',lab),
                     match('BSJF',lab)))
p <- length(interestingIdxs)
lab[interestingIdxs]
meanv[interestingIdxs]
sdevv[interestingIdxs]
justLab <- c(lab[interestingIdxs],'^GSPC')
sAndPprices <- getHistPrices(c('^GSPC'),c(1.0),len,
                             start=start,end=end)
justPrices <- cbind(prices[,interestingIdxs],sAndPprices)
p <- p + 1
plotMultSeries(justPrices,justLab,rep(1/p,p),p,ylim=c(.9,2.8))

lab[interestingIdxs]
meanv[interestingIdxs]
sdevv[interestingIdxs]
t(cbind(lab[w > 0],w[w > 0]))
portv[len]
sp[len]
portv[len]-sp[len]
