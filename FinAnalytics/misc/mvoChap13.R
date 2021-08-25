library(tseries)
destLab <- unique(ISptrnDF$symbol)
obsDays <- 82

#Set up date ranges and obtain quotes for S&P:
par(mfrow=c(1,3))
start2 = "2013-01-21"
end2   = "2013-05-20"
pricesSP2 <- getHistPrices(c("^GSPC"),c(1),obsDays,start=start2,end=end2)
plot(pricesSP2,type="l",ylim=c(1400,2400))
start1 = "2014-01-22"
end1   = "2014-05-20"
pricesSP1 <- getHistPrices(c("^GSPC"),c(1),obsDays,start=start1,end=end1)
plot(pricesSP1,type="l",ylim=c(1400,2400))
start0 = "2015-01-22"
end0   = "2015-05-20"
pricesSP0 <- getHistPrices(c("^GSPC"),c(1),obsDays,start=start0,end=end0)
plot(pricesSP0,type="l",ylim=c(1400,2400))

#Retrieve market prices for 3 separate years:
res <- findAllPrices("MVO3.2013.05") #Uses Python acquireCoalescedPrices()
prices2 <- res[[1]]; lab2 <- res[[2]]
prices2 <- prices2[(nrow(prices2)-(obsDays-1)):nrow(prices2),]

res <- findAllPrices("MVO3.2014.05") #Uses Python acquireCoalescedPrices()
prices1 <- res[[1]]; lab1 <- res[[2]]
prices1 <- prices1[(nrow(prices1)-(obsDays-1)):nrow(prices1),]

res <- findAllPrices("MVO3.2015.05") #Uses Python acquireCoalescedPrices()
prices0 <- res[[1]]; lab0 <- res[[2]]
prices0 <- prices0[(nrow(prices0)-(obsDays-1)):nrow(prices0),]

#Utility: Find indexes of destLab in lab:
findIndexes <- function(destLab,lab){
  idxVec <- c()
  for(l in destLab) {
    idx <- match(l,lab)
    idxVec <- c(idxVec,idx)
  }
  return(idxVec)
}
#Be able to use: meanvAbvSP2[lab2Idxs] etc. to find mean above S&P
lab2Idxs <- findIndexes(destLab,lab2)
lab1Idxs <- findIndexes(destLab,lab1)
lab0Idxs <- findIndexes(destLab,lab0)

R2 <- findR(prices2[,lab2Idxs])
R1 <- findR(prices1[,lab1Idxs])
R0 <- findR(prices0[,lab0Idxs])
r2 <- findR(as.matrix(pricesSP2,obsDays,1))
r1 <- findR(as.matrix(pricesSP1,obsDays,1))
r0 <- findR(as.matrix(pricesSP0,obsDays,1))

findOneYrPriceStats <- function(R,r) {
  #Go back 3 years mean log ret and sdev
  meanSP     <- apply(r,2,mean)
  meanvAbvSP <- apply(R,2,mean)-meanSP
  meanv      <- apply(R,2,mean)
  cov_mat    <- cov(R/100) #rescale back to logret wo 100 factor
  diag_cov_mat <- diag(cov_mat)
  sdevv      <- sqrt(diag_cov_mat)
  SR         <- meanvAbvSP/sdevv
  return(list(meanvAbvSP,sdevv))
}
res <- findOneYrPriceStats(R2,r2)
meanvAbvSP2 <- res[[1]]
sdevv2      <- res[[2]]
res <- findOneYrPriceStats(R1,r1)
meanvAbvSP1 <- res[[1]]
sdevv1      <- res[[2]]
res <- findOneYrPriceStats(R0,r0)
meanvAbvSP0 <- res[[1]]
sdevv0      <- res[[2]]
meanvAbvSP = c(meanvAbvSP0,meanvAbvSP1,meanvAbvSP2)
summary(meanvAbvSP)
par(mfrow=c(1,3))
plot(meanvAbvSP2)#[lab2Idxs])
plot(meanvAbvSP1)#[lab1Idxs])
plot(meanvAbvSP0)#[lab0Idxs])
#Predicate involving CNI stock 1st log ret and 1st R value
abs(diff(log(prices0[,lab0Idxs][1:2,match("CNI",lab0[lab0Idxs])]))*100
    - R0[1,match("CNI",lab0[lab0Idxs])]) < .001
#Compute CNI stock mean above SP and compare to already computed one:
abs(mean(diff(log(prices2[,lab2Idxs][,337]))*100-diff(log(pricesSP2))*100)-
    meanvAbvSP2[337]) < .00001

#SBUX prices: this result matches google finance quotes for SBUX and S&P
match("SBUX",lab1[lab1Idxs])
plot(prices1[,lab1Idxs][,1589],type="l")
mean(diff(log(prices1[,lab1Idxs][,1589]))*100
     -diff(log(pricesSP1))*100)

augPtrnByGroup <- function(ISptrnDF,labIdxs,meanvAbvSP,sdevv,groupNum) {
  #augment DF with price stats
  N <- floor(dim(ISptrnDF)[1]/3)
  if(groupNum == 1) {
    ISptrnDF[1:N,c(8,9)] <- cbind(round(meanvAbvSP,4),round(sdevv,4))
      #cbind(round(meanvAbvSP[labIdxs],4),round(sdevv[labIdxs],4))
  } else if(groupNum == 2) {
    ISptrnDF[(N+1):(2*N),c(8,9)] <-
      cbind(round(meanvAbvSP,4),round(sdevv,4))  
      #cbind(round(meanvAbvSP[labIdxs],4),round(sdevv[labIdxs],4))    
  } else if(groupNum == 3) {
    ISptrnDF[(2*N+1):(3*N),c(8,9)] <-
      cbind(round(meanvAbvSP,4),round(sdevv,4))       
      #cbind(round(meanvAbvSP[labIdxs],4),round(sdevv[labIdxs],4))    
  }
  ISptrnDF
}
ISptrnDF <- augPtrnByGroup(ISptrnDF,lab2Idxs,meanvAbvSP2,sdevv2,groupNum=1)
ISptrnDF <- augPtrnByGroup(ISptrnDF,lab1Idxs,meanvAbvSP1,sdevv1,groupNum=2)
ISptrnDF <- augPtrnByGroup(ISptrnDF,lab0Idxs,meanvAbvSP0,sdevv0,groupNum=3)
ISptrnDFcln <- na.omit(ISptrnDF)
print(dim(ISptrnDFcln))
D <- match('ZUMZ',ISptrnDFcln$symbol)
Dvec <- which(ISptrnDFcln$symbol=="ZUMZ")

library(moments)
yb2logrets <- ISptrnDFcln[1:Dvec[1],8]
yb1logrets <- ISptrnDFcln[(Dvec[1]+1):(Dvec[2]),8]
yb0logrets <- ISptrnDFcln[(Dvec[2]+1):(Dvec[3]),8]
alllogrets <- c(yb2logrets,yb1logrets,yb0logrets)
skewness(alllogrets)
kurtosis(alllogrets)
par(mfrow=c(1,1))
plot(density(yb2logrets),main="")
lines(density(yb1logrets),col=4)
lines(density(yb0logrets),col=9)
abline(v=0.0)
summary(alllogrets)
skewness(alllogrets)
kurtosis(alllogrets)

HL=ifelse(ISptrnDFcln$meanabv > 0,"Up>0","Down<=0")
IStreeDF = data.frame(ISptrnDFcln,HL)
IStreeDF[IStreeDF$symbol=="SBUX",]

hist(IStreeDF$netincgth[1:Dvec[1]],breaks=200)
mean(IStreeDF$netincgth[1:Dvec[1]],trim=.02)
mean(IStreeDF$meanabvsp[1:Dvec[1]],trim=.02)

hist(IStreeDF$netincgth[(Dvec[1]+1):Dvec[2]],breaks=200)
mean(IStreeDF$netincgth[(Dvec[1]+1):Dvec[2]],trim=.02)
mean(IStreeDF$meanabvsp[(Dvec[1]+1):Dvec[2]],trim=.02)

hist(IStreeDF$netincgth[(Dvec[2]+1):Dvec[3]],breaks=200)
mean(IStreeDF$netincgth[(Dvec[2]+1):Dvec[3]],trim=.02)
mean(IStreeDF$meanabvsp[(Dvec[2]+1):Dvec[3]],trim=.02)

if(TRUE) {
  setwd("..")
  getwd()
  write.csv(IStreeDF,file="ISDFv5.csv",row.names=FALSE)
}

convISgthDFbasedate <- function(fin,fout){
  df <- read.csv(fin,stringsAsFactors = F)
  head(df)
  df$basedate <- as.Date(df$basedate)
  df$basedate[df$yrsback==2] <- df$basedate[df$yrsback==2]-365*2
  df$basedate[df$yrsback==1] <- df$basedate[df$yrsback==1]-365
  print(df[df$symbol=="NVDA",])
  write.csv(df,row.names = F,file=fout)
}
#Correct basedate issue in ISgthDF:
convISgthDFbasedate("ISDFv5.csv","ISDFv6.csv")
