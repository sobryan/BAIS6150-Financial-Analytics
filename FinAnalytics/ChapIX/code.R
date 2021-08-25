#Chapter IX
daysPerYr = 252
D <- NA

findRecentHugePrices <- function(dir,portFile) {
  #Take portfolio from portFile and find recent prices in cache.
  #Side effects Lab, W, D, len
  setwd(paste(homeuser,"/FinAnalytics/",dir,"/",sep=""))
  df   <- read.csv(portFile)
  lab  <<- df[,2] #lab[2] is no longer expected to be FBMI
  w    <<- df[,3]
  indw <- (w > 0)
  lab  <- lab[indw]
  w    <- w[indw]
  D    <<- length(lab)
  len  <<- daysPerYr*6
  prices = matrix(rep(NA,len*D),nrow=len,ncol=D)
  #We have cache 2008 to 2014 prices in MVO6 dir
  dir = 'MVO6'
  d = 1
  for(l in lab) {
    fileName = paste('cached',l,'.csv',sep='')
    for(subdir in c('NYSE','NASDAQ')) {
      setwd(paste(homeuser,"/FinAnalytics/",dir,'/',
                  subdir,sep=''))
      if(file.exists(fileName))
        break
    }
    print(fileName)
    prices[,d] = read.csv(fileName,header=TRUE,
                          sep='')[,1]
    d = d + 1
  }
  #Validation of prices exist
  for(d in 1:D)
    if(is.na(prices[1,d]))
      stop(lab[d])
  plotMultSeries(prices,lab,w,D,ylim=c(.7,13))
  return(prices)
}
#This fails:
prices <- findRecentHugePrices('huge','resD26QP1Days1258.csv')
#This fails:
prices <- findRecentHugePrices('huge','resD25Days1258woTIE.csv')
#success:
prices <- findRecentHugePrices('huge','rebalresD24Days1258.csv')

dim(prices)
library(stats)
#K-means clustering
findMeanForYrs <- function(prices) {
  D <- dim(prices)[2]
  R <- findR(prices)
  meanLogRet = matrix(nrow=6,ncol=D)
  for(j in 1:D) { #security j
    R[,j] = 100*diff(log(prices[,j]))
    for(l in 1:6) { #year l
      meanLogRet[l,j] = 1/(daysPerYr-1)*
        sum(R[(2+(l-1)*daysPerYr-1):(l*daysPerYr-1),j])
    }
  }
  meanLogRet
}
meanLogRet <- findMeanForYrs(prices)
meanLogRet <- findMeanForYrs(prices)
colnames(meanLogRet) <- lab
rownames(meanLogRet) <- c(2008,2009,2010,
                          2011,2012,2013)
round(meanLogRet[,1:4],4)
#sample first 4
meanLogRetT = t(meanLogRet)
round(meanLogRetT[1:4,],2)

set.seed(1) #This kmeans call is based upon mean
grpMeanLogRet2 <- kmeans(meanLogRetT, centers=2, nstart=10)
sort(grpMeanLogRet2$cluster)
                
grpMeanLogRet3 <- kmeans(meanLogRetT, centers=3, nstart=10)
sort(grpMeanLogRet3$cluster)
        
grpMeanLogRet4 <- kmeans(meanLogRetT, centers=4, nstart=10)
sort(grpMeanLogRet4$cluster)

round(meanLogRetT[match('AAPL',lab),],2)
sum(meanLogRetT[match('AAPL',lab),])
round(meanLogRetT[match('PCLN',lab),],2)
sum(meanLogRetT[match('PCLN',lab),])

grpMeanLogRet5 <- kmeans(meanLogRetT, centers=5, nstart=10)
sort(grpMeanLogRet5$cluster)

#Now use entire time series mean and sd
R <- findR(prices)
cov_mat = cov(R/100) #rescale R back to orig sz.
mean_vect = apply(R,2,mean)
diag_cov_mat = diag(cov_mat)
sd_vect = sqrt(diag_cov_mat)*sqrt(daysPerYr)
meanLogRetVolByStockDF <-
  data.frame(ticker=colnames(meanLogRet),
             mean=mean_vect, sdev=sd_vect)

meanLogRetVolByStockDF[1:5,]
set.seed(1) #This kmeans call is based on mean log ret and vol
grpMeanLogRetVol <-
  kmeans(meanLogRetVolByStockDF[,c("mean","sdev")],
         centers=5, nstart=10)
o = order(grpMeanLogRetVol$cluster)
data.frame(meanLogRetVolByStockDF$ticker[o],
           grpMeanLogRetVol$cluster[o])

par(mfrow=c(1,1))
plotMeans <- function(x,y,tickers,cluster,
                      centers) {
  par(mar=c(4,4,2.82,2.82))
  plot(x,y,type='n',
       xlim=c(0.1,.75),ylim=c(-.04,.24),
       ylab="avg ret",xlab="vol")
  text(x,y,labels=tickers,
       col=(cluster+1),cex=.55)
  points(centers[,2],centers[,1],cex=6.0,col=4)
  lines(x=c(.1,.25),y=c(.1,.25))
}
plotMeans(meanLogRetVolByStockDF$sdev,
          meanLogRetVolByStockDF$mean,
          meanLogRetVolByStockDF$ticker,
          grpMeanLogRetVol$cluster,
          grpMeanLogRetVol$centers)

l2dist <- function(x,y) {
  sqrt((x[1]-y[1])^2 + (x[2]-y[2])^ 2)
}
#unit test
l2dist(c(3,4),c(0,0)) == 5

p = dim(meanLogRetVolByStockDF)[1]
k = 5
logRetVolWMeanDistDF <- data.frame(
  as.character(meanLogRetVolByStockDF[,1]),
  meanLogRetVolByStockDF[,2],
  meanLogRetVolByStockDF[,3],
  rep(0,p))
colnames(logRetVolWMeanDistDF) <-
  c("ticker","mean","sdev","jthMeanIdx")
logRetVolWMeanDistDF

#Initial: Randomly choose k units as cluster means first
set.seed(46510)
idxs <- sample(1:p, k)
clusterMeans <- matrix(
  c(meanLogRetVolByStockDF[idxs,2],
    meanLogRetVolByStockDF[idxs,3],
    idxs),nrow=5,ncol=3)
clusterMeans
newStepClusterMeans <- matrix(clusterMeans,
                       nrow=5,ncol=3) #clone initially
par(mfrow=c(2,2))

kmeansSteps <- function() {
  for(t in 1:4) {
    if(sum(is.na(clusterMeans)) > 1) stop
    #Assignment step:
    if(t > 1)
      for(i in 1:p) {#find closest mean for i-th ticker
        min_l2dist <- 1e6 #start off w/infinity
        for(j in 1:k) {
          x1 <- logRetVolWMeanDistDF[i,2]
          x2 <- logRetVolWMeanDistDF[i,3]
          x <- c(x1,x2)
          m <- clusterMeans[j,1:2]
          l2dist_x_m <- l2dist(x,m)
          if(l2dist_x_m <= min_l2dist) {
            min_l2dist <- l2dist_x_m
            best_j <- j
          }
        }
        logRetVolWMeanDistDF[i,4] <- best_j
      }
    else
      logRetVolWMeanDistDF[,4] <- sample(1:k, p, replace=TRUE)
    print(t(logRetVolWMeanDistDF[,c(1,4)]))
    #Update step:
    for(j in 1:k) {
      print(paste("update step j =",j))
      x1ClusterMean <- mean(
        logRetVolWMeanDistDF[logRetVolWMeanDistDF$jthMeanIdx==j,2])
      x2ClusterMean <- mean(
        logRetVolWMeanDistDF[logRetVolWMeanDistDF$jthMeanIdx==j,3])
      newStepClusterMeans[j,1:2] <-
        c(x1ClusterMean,x2ClusterMean)
      newStepClusterMeans[j,3] <- TRUE #not needed now
    }
    print(newStepClusterMeans)
    plotMeans(logRetVolWMeanDistDF$sdev,
              logRetVolWMeanDistDF$mean,
              logRetVolWMeanDistDF$ticker,
              logRetVolWMeanDistDF$jthMeanIdx,
              newStepClusterMeans)
    points(clusterMeans[,1]~
             clusterMeans[,2],cex=9,col=8)
    points(newStepClusterMeans[,1]~
             newStepClusterMeans[,2],cex=9,col=9)
    clusterMeans <- newStepClusterMeans
  }
}
kmeansSteps()

computeSparsity <- function(A) {
  dimA = dim(A)
  if(dimA[1] == dimA[2]) {
    sumedges = 0
    p = dimA[1]
    for(i in 1:p)
      if(i > 1)
        for(j in 1:(i-1))
          sumedges = sumedges + A[i,j]
  } else return(NA)
  return(1-sumedges/((p*(p-1)/2)))
}
cells = c(0,0,1,1,
          0,0,1,1,
          1,1,0,1,
          1,1,1,0)
A = matrix(cells,nrow=4,ncol=4)
computeSparsity(A)
1-computeSparsity(A) #density

computeClusterCoeff <- function(A, isVerbose=FALSE) {
  N = dim(A)[1]
  degree = vector(length=dim(A)[1])
  avgdegree = vector(length=dim(A)[1])
  sumCC = 0
  for (i in 1:N) {
    sum = 0
    degree[i] = sum(A[i,])
    avgdegree[i] = degree[i]*(degree[i]-1)/2
    if(degree[i] < 2) {
      avgdegree[i] = 1; sum = 1
    } else {
      avgdegree[i] = dim(combn(degree[i],2))[2]
      for(j in 1:N) {
        for(k in j:N) {
          fact = A[i,j]*A[j,k]*A[k,i]
          if(fact > 0) {
            sum = sum + fact
            #print(paste(i,j,k,fact))
          }
        }
      }
    }
    if(isVerbose) print(paste(i,"===> cc num =",sum))
    if(avgdegree[i] != 0) {
      if(isVerbose) print(paste(i,
                                "===> clst coeff =",sum/avgdegree[i]))
      sumCC = sumCC + sum/avgdegree[i]
    }
  }
  sumCC/N
}
#Unit test
cells = c(0,0,1,1,
          0,0,1,1,
          1,1,0,1,
          1,1,1,0)
A = matrix(cells,nrow=4,ncol=4)
computeClusterCoeff(A,isVerbose=TRUE)

library(igraph)
library(tseries)
plotGraph <- function(lab,w,A) {
  D = dim(A)[1]
  indw = (w > .001)
  g <- graph.empty() + vertices(toupper(lab[indw]))
  threshold = .6
  for(i in 1:D) {
    if(w[i] > 0.0) {
      for(j in 1:max(1,(i-1))) {
        if(i != j && w[j] > .001 && A[i,j] != 0) {
          #print(toupper(lab[j]))
          g <- g + path(toupper(lab[i]),toupper(lab[j]))
        }
      }
    }
  }
  ug <- as.undirected(g)
  V(ug)$color <- "gold"
  V(ug)$label.cex = 1.1
  plot(ug,vertex.size=22.05)
}
plotGraph(c('W','X','Y','Z'),c(1/4,1/4,1/4,1/4),A)

#Find covariance and precision Matrices
library(huge)
data(stockdata)
D = length(stockdata$data[1,])
len = length(stockdata$data[,1])
prices = stockdata$data[,1:D]
lab = stockdata$info[1:D,1]
isSplitAdjusted = FALSE
R <- findR(prices,isSplitAdjusted=FALSE) #Split-adjusts prices
dim(prices)
#Form small p array of prices
ticker = c('MCD','MON','PCP','PCLN')
matchIdx = vector(length=4)
for(i in 1:4)
  matchIdx[i] = match(ticker[i],lab)
p = matrix(rep(0,252*4),nrow=252,ncol=4)
oneYr = (1258-251):1258
p[,1] = prices[oneYr,matchIdx[1]]
p[,2] = prices[oneYr,matchIdx[2]]
p[,3] = prices[oneYr,matchIdx[3]]
p[,4] = prices[oneYr,matchIdx[4]]

r = matrix(rep(0,251*4),nrow=251,ncol=4)
r[,1] = diff(log(p[,1]))
r[,2] = diff(log(p[,2]))
r[,3] = diff(log(p[,3]))
r[,4] = diff(log(p[,4]))
r100 = 100*r #100 x log rets
Sigma = cov(r100)
round(Sigma,2)
Omega = solve(Sigma)
round(Omega,2)
A = ifelse(round(Omega,2)!=0.00, 1, 0)
w = c(.25,.25,.25,.25)
plotGraph(ticker,w,A)
plotMultSeries(p,ticker,w,4,
               cc=paste(sum(w>0),"stocks"),
               ret="",ylim=c(.8,3))

findSixYrSR <- function(dir='huge',csvFile = 'rebalresD24Days1258.csv') {
  setwd(paste(homeuser,"/FinAnalytics/",dir,"/",sep=""))
  df <- read.csv(csvFile)
  lab <- df[,2]
  w <- df[,3]
  indw = (w > 0)
  lab <- lab[indw]
  isEnhanced <- FALSE
  w <- w[indw]
  D <- length(lab)
  daysPerYr = 252; mufree = 0
  recentPrices <- findRecentHugePrices('huge',
                                       'rebalresD24Days1258.csv')
  R <- findR(recentPrices)
  cov_mat <- cov(R)
  meanv <- apply(R,2,mean)
  diag_cov_mat <- diag(cov_mat)
  sdevv <- sqrt(diag_cov_mat)
  Sharpe <- (meanv-mufree)/sdevv*sqrt(daysPerYr)
  Omega <- solve(cov_mat)
  prices <- recentPrices
  list(prices,R,cov_mat,meanv,sdevv,Sharpe,Omega,isEnhanced)
}
res <- findSixYrSR()
prices     <- res[[1]]
R          <- res[[2]]
cov_mat    <- res[[3]]
meanv      <- res[[4]]
sdevv      <- res[[5]]
Sharpe     <- res[[6]]
Omega      <- res[[7]]
isEnhanced <- res[[8]]

enhanceLab <- function(lab,Sharpe,w) {
  #Enhance lab with Sharpe and weight in percent
  D <- length(lab)
  shplab = vector(length=D)
  for(d in 1:D) {
    shplab[d] = paste(lab[d],
                      paste(round(Sharpe[d],2),
                            paste(round(100*w[d],0),'%',sep='')),sep='\n')
  }
  return(shplab)
}
shplab <- enhanceLab(lab,Sharpe,w)
shplab

runGlassoAndDisplay <- function(prices,lab,w,D,Sharpe,
                       isEnhanced=FALSE,lmratio = 0.33,trackIdx=9) {
  #Run the Glasso and record results in undir graph ug
  len = length(prices[,1]) # Does not impact R:
  Y = log(prices[2:len,1:D]/prices[1:(len-1),1:D])
  x.npn = huge.npn(Y, npn.func="truncation") # Nonparanormal
  out.npn = huge(x.npn,method = "glasso",
                 cov.output = TRUE, nlambda=D,
                 lambda.min.ratio = lmratio)
  out.npn
  #Find indicator array:
  indw = (w > .001)
  #Attach SR to lab
  if(!isEnhanced && D > 4) {
    shplab <- enhanceLab(lab,Sharpe,w)
    isEnhanced <- TRUE #shplab enhanced: e.g. "ISRG\n0.14 4%"
  }
  g <- graph.empty() + vertices(toupper(shplab[indw]))
  trackIdxEdges <- 0 #Track MCD
  for(d in D:D) { #focus on last version D
    for(i in 1:D) {
      if(w[i] > .001) {
        for(j in 1:i) {
          if(w[j] > .001 && out.npn$path[[d]][i,j] == 1) {
            #print(paste(i,j))
            #print(toupper(lab[i]))
            g <- g + path(toupper(shplab[i]),toupper(shplab[j]))
            #Undir graph means need to count either case:
            if(j == trackIdx || i == trackIdx)
              trackIdxEdges <- trackIdxEdges + 1
          }
        }
      }
    }
    ug <- as.undirected(g)
    V(ug)$color <- "gold"
    #V(ug)$offset <- 1.2
    V(ug)$label.cex = 0.8
    plot(ug,vertex.size=sqrt(500*w),ylab=
           paste("lmratio=",lmratio))
  }
  print(paste("tracked outdegree:",trackIdxEdges))
  list(out.npn$path[[D]],shplab,isEnhanced) 
}

#Need to refind Omega for 4x4 case:
Omega = round(solve(Sigma),2)
A = ifelse(Omega!=0.00, 1, 0)
lab4 <- c('MCD','MON','PCP','PCLN')
labIdxs <- sapply(lab4,function(x) match(x,lab))
prices4 <- prices[,labIdxs]
w = rep(1/4,4)
shplab <- lab[labIdxs]
par(mfrow=c(2,2))
res <- runGlassoAndDisplay(prices4,lab4,w,4,Sharpe,
                        lmratio=1.20,trackIdx=1)
A   <- res[[1]]
shplab <- res[[2]]
isEnhanced <- res[[3]]
res <-runGlassoAndDisplay(prices4,lab4,w,4,Sharpe,
                        lmratio=.95,trackIdx=1)
A   <- res[[1]]
shplab <- res[[2]]
isEnhanced <- res[[3]]
res <- runGlassoAndDisplay(prices4,lab4,w,4,Sharpe,
                        lmratio=.70,trackIdx=1)
A   <- res[[1]]
shplab <- res[[2]]
isEnhanced <- res[[3]]
res <- runGlassoAndDisplay(prices4,lab4,w,4,Sharpe,
                        lmratio=.45,trackIdx=1)
A   <- res[[1]]
shplab <- res[[2]]
isEnhanced <- res[[3]]

w = rep(1/D,D)
Omega = round(solve(cov_mat),2)
Omega[1:8,1:8]
Aomega = ifelse(Omega!=0.00, 1, 0)
res <- runGlassoAndDisplay(prices,lab,w,D,Sharpe,
                        lmratio=.45,trackIdx=4)
A   <- res[[1]]
shplab <- res[[2]]
isEnhanced <- res[[3]]
Aomega[1:8,1:8]
huge.plot(A)

#Avg Cov
saveList <- list(D,lab,prices) #save away
library(huge)
data(stockdata)
D = length(stockdata$data[1,])
len = length(stockdata$data[,1])
prices = stockdata$data[,1:D]
lab = stockdata$info[1:D,1]
isSplitAdjusted = FALSE

R <- findR(prices,isSplitAdjusted=FALSE) #Split-adjusts prices
dim(prices)
cov_mat <- cov(R)
sumcovv=vector(length=D)
for(i in 1:D)
  sumcovv[i]=sum(cov_mat[i,])-cov_mat[i,i]
plot(sumcovv/D,type="p",cex=.1)
text(1:D,sumcovv/D,lab,col=4,cex=.55)

lab4 <- c('EBAY','EMC','PCLN','UPS')
labIdxs <- sapply(lab4,function(x) match(x,lab))
plot(sort(cov_mat[139,-139]),ylim=c(-.1,4),
     xlab="Sorted Index",ylab="Cov to other stocks")
points(sort(cov_mat[145,-145]),col=2)
points(sort(cov_mat[338,-338]),col=4)
points(sort(cov_mat[417,-417]),col=3)
text(rep(400,4),c(1.1,1.75,2.5,.5),lab4,cex=.75)
D <- saveList[[1]]; lab <- saveList[[2]]; prices <- saveList[[3]]#restore

mapToCol <- function(d)
  if(d%%8==7) 1 else if(d==8)
    2 else if(d==15) 3 else if(d==23) 4 else d
library(sbgcop)
Omega = matrix(c(0.86, -0.15, -0.07, 0.00,
                 -0.15, 0.35, -0.13, -0.04,
                 -0.07, -0.13, 0.35, -0.04,
                 0.00, -0.04, -0.04, 0.12),nrow=4,ncol=4)
A = ifelse(Omega!=0.00, 1, 0)
plotGraph(c('MCD','MON','PCP','PCLN'),rep(1/4,4),A)
Sig= solve(Omega)
p <- dim(Sig)[1]
df <- p+1

set.seed(138) # for replication
paths <- 100 # number of obs in our sampling dist
W.empir <- matrix( nrow = paths, ncol = length( c(Sig) ) )
dim(W.empir)
for(i in 1:paths) {
  W.empir[i, ] <- c(rwish(Sig,nu=1))
  if(i == 1) {
    plot(as.vector(W.empir[i,]),type="l",
         ylim=c(-15,+90),ylab="rwish npaths=100")
  } else {
    lines(as.vector(W.empir[i,]),col=mapToCol(i))
  }
}
boxplot(W.empir)
meanW <- apply(W.empir,2,function(x) mean(as.vector(x)))
matrix(round(meanW,2),4,4)
round(Sig,2)

for(j in (1:16)) {
  if(j == 1) {
    plot(density(W.empir[j,]),
         xlim=c(min(W.empir),max(W.empir)),
         ylim=c(0,.8),main="")
  } else {
    lines(density(W.empir[j,]),col=mapToCol(j))
  }
}

prices[252,4]/prices[1,4]-1
prices[2*252,4]/prices[253,4]-1
prices[3*252,4]/prices[252*2+1,4]-1
prices[4*252,4]/prices[252*3+1,4]-1
prices[5*252,4]/prices[252*4+1,4]-1
prices[6*252,4]/prices[252*5+1,4]-1

runSixYrsGlasso <- function(daysPerPeriod,Sharpe,y=NA,sleepIntval=0,
                            isClusterCoeff=TRUE) {
  #Run Glasso alg from 2008 to 2014 by yr, qtr, mo
  totalPeriods= 6*daysPerYr/daysPerPeriod
  par(mfrow=c(1,1))
  sparsity = array(dim = c(totalPeriods))
  clustCoeff = array(dim = c(totalPeriods))
  portv = array(dim = c(totalPeriods))
  if(is.na(y)) yrange = c(1:totalPeriods) else yrange = c(y:y)
  for(y in yrange) { #2008:2009 to 2013:2014
    d1 = (y-1)*daysPerPeriod+1
    d2 = y*daysPerPeriod
    print(d1);print(d2)
    res <- runGlassoAndDisplay(prices[d1:d2,],lab,w,D,Sharpe,
                            lmratio=.6,trackIdx=4)
    A <- res[[1]]
    sparsity[y] <- round(computeSparsity(A),4)
    if(isClusterCoeff)
      clustCoeff[y] <- round(computeClusterCoeff(A),4)
    #compute portfolio return:
    portValue <- round( w %*% (prices[d2,]/prices[d1,]), 4)
    portv[y] <- portValue[1,1]
    Sys.sleep(sleepIntval)
    if(daysPerPeriod == 252) { #yearly case
      if(y == 2) ylim = c(.5,3.1) else ylim = c(.2,2.2)
      plotMultSeries(prices[d1:d2,],lab,w,D,cc=sparsity[y],
                     ret=portV[1,1],ylim=ylim,isAlone=TRUE)
    } else {
      portvDetail = array(rep(0,daysPerPeriod),
                          dim = c(daysPerPeriod))
      for(d in 1:D)
        portvDetail = portvDetail +
            w[d] * (prices[d1:d2,d]/prices[d1,d])
      plot(portvDetail,type='l',xlab="year",
          ylab="portolio value")
    }
    Sys.sleep(sleepIntval)
  }
  return(list(sparsity,clustCoeff,portv))
}
res <- runSixYrsGlasso(21,Sharpe,y=1)[[1]] #1 mo run

glassoRes <- runSixYrsGlasso(252,Sharpe,sleepIntval=10)

yrlySparsity = glassoRes[[1]]
yrlySparsity
yrlyClustCoeff = glassoRes[[2]]
yrlyClustCoeff
yrlyPortV = glassoRes[[3]]
yrlyPortV

mapToYr <- function(per,mode=1) {
  if(mode==1) per+2007 else if(mode==2) per/4+2008
  else (per+2)/12+2008 }

fitLinReg <- function(sparsity,clustCoeff,portv,
                      daysPerPeriod,mode=1,LRTerms=3) {
  totalPeriods = 6*daysPerYr/daysPerPeriod
  periodsByYr = mapToYr(c(1:totalPeriods),mode=mode)
  shiftedPortV = c(1,portv[1:(totalPeriods-1)])
  if(LRTerms == 3) {
    lm <- lm(shiftedPortV ~ sparsity + clustCoeff + portv)
  } else if(LRTerms == 2) {
    lm <- lm(shiftedPortV ~ sparsity + clustCoeff)
  } else {
    lm <- lm(shiftedPortV ~ sparsity)
  }
  print(summary(lm))
  coef <- coef(lm)
  coef
  beta0 = coef[1]
  print(beta0)
  beta1 = coef[2]
  print(beta1)
  if(LRTerms >= 2) beta2 = coef[3]
  if(LRTerms == 3) beta3 = coef[4]
  if(LRTerms == 3)
    z = beta0 + beta1*sparsity +
    beta2*clustCoeff + beta3*portv
  else if(LRTerms == 2)
    z = beta0 + beta1*sparsity +
    beta2*clustCoeff
  else
    z = beta0 + beta1*sparsity
  z[1] = 1.0
  par(mar=c(4,4,2.82,2.82))
  par(mfrow=c(1,1))
  plot(periodsByYr,sparsity,type='l',
       col=2,ylim=c(.2,1.5),xlab="year")
  points(periodsByYr,sparsity,col=2)
  if(LRTerms > 1) {
    lines(periodsByYr,clustCoeff,type='l',col=5)
    points(periodsByYr,clustCoeff,col=5)
  }
  lines(periodsByYr,shiftedPortV,type='l',col=4)
  points(periodsByYr,shiftedPortV,col=4)
  lines(periodsByYr,z,col=27)
  lines(periodsByYr,rep(1,totalPeriods))
  indz = (z>=1)
  indNonNegV = (shiftedPortV>=1)
  print((sum(indNonNegV == indz)-1)/(length(indz)-1))
  for(y in 2:totalPeriods)
    if(indz[y] != indNonNegV[y]) {
      lines(c(z[y],shiftedPortV[y])~
        c(mapToYr(y,mode=mode),mapToYr(y,mode=mode)),col="red")}
  return(data.frame(z,sparsity,clustCoeff,portv,shiftedPortV))
}

runGlassoAndLinReg <- function(daysPerPeriod,Sharpe,
                               mode=1,LRTerms=1) {
  totalPeriods = 6*daysPerYr/daysPerPeriod
  glassoRes = runSixYrsGlasso(daysPerPeriod,Sharpe)
  sparsity = glassoRes[[1]]
  clustCoeff = glassoRes[[2]]
  portv = glassoRes[[3]]
  lrres <- fitLinReg(sparsity,clustCoeff,portv,
                     daysPerPeriod,mode=mode,LRTerms=LRTerms)
  lrres
}
yrlyDF  <- runGlassoAndLinReg(252,Sharpe,mode=1,LRTerms=1)
qtrlyDF <- runGlassoAndLinReg(63,Sharpe,mode=2,LRTerms=1)
mthlyDF <- runGlassoAndLinReg(21,Sharpe,mode=2,LRTerms=1)
