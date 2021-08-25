#General script for N years back
rm(list=ls())

findTopSharpes <- function(indepSharpes,N) {
  #Go from 1 down to 0 and find top Sharpes:
  SRrange <- seq(1,0,-.0025)
  indepSharpes[is.na(indepSharpes)] <- 0.0 #Force low Sharpe
  for(sr in SRrange) {
    indHighSharpes <- (indepSharpes > sr)
    if(F) print(sr)
    if(sum(indHighSharpes) >= N) {
      print(paste("sr =",sr))
      print(indepSharpes[indHighSharpes])
      return(list(indHighSharpes,sr))
    }
  }
}
#unit test:
resTS <- findTopSharpes(indepSharpes=seq(.9,0,-.1),N=5)
resTS[[1]]

pruneBySharpe <- function(prices,lab,meanv,sdevv,threshSR,mufree=0) {
  par(mar=c(4,4,1,1))
  par(mfrow=c(1,2))
  indepSharpes <- (meanv-mufree)/sdevv
  len = length(indepSharpes)
  plot(indepSharpes,ylab="SR",col=4)
  plot(sort(indepSharpes),ylab="SR",col=4)
  lines(1:len,rep(threshSR,len))
  print(paste("threshSR =",threshSR))
  if(threshSR < 1.0) {
    indHighSharpes <- (indepSharpes > threshSR)
    abline(h=threshSR)
  } else {
    res <- findTopSharpes(indepSharpes,N=threshSR)
    indHighSharpes <- res[[1]]; sr <- res[[2]]
    abline(h=sr)
  }
  #clean up NAs
  for(d in 1:length(indHighSharpes)) #clean up NAs
    if(is.na(indHighSharpes[d]))
      indHighSharpes[d] <- FALSE
  len = dim(prices)[1]
  wid = dim(prices)[2]
  smallerSz = sum(indHighSharpes)
  newPrices <- matrix(rep(0,len*smallerSz),
                      nrow=len,ncol=smallerSz)
  newLab    <- vector(length=smallerSz)
  e <- 1
  for(d in 1:wid) {
    if(indHighSharpes[d]) {
      print(paste("e",e))
      newPrices[,e] <- prices[,d]
      newLab[e] <- lab[d]
      e <- e + 1
    }
  }
  print("completed Sharpe pruning")
  list(newPrices,newLab,indepSharpes)
}

#test: dir <- 'MVO3.2017.11'
acquireCoalescedPrices <- function(dir,isSubDir=TRUE) {
  utilName <- paste0("python3 ",homeuser,
          "/PycharmProjects/coalescePrices/coalescePrices.py ")
  
  if(isSubDir) { path <- paste0(homeuser,"/FinAnalytics/",dir,"/NYSE/") }
  else { path <- paste0(homeuser,"/FinAnalytics/",dir)  }
  print(paste("acquireCoalescedPrices path =",path))
  
  if(!file.exists(paste0(path,"prices.csv")))
    system(paste0(utilName,path))
  setwd(path)
  mat1 <- read.csv("prices.csv",
                   sep=',',header=F,stringsAsFactors=F)
  lab1 <- as.character(mat1[1,])
  prices1 <- apply(
    as.matrix(mat1[2:nrow(mat1),],nrow=(nrow(mat1)-1),ncol=ncol(mat1)),
    c(1,2),as.numeric)
  if(isSubDir) {
    path <- paste0(homeuser,"/FinAnalytics/",dir,"/NASDAQ/")
    if(!file.exists(paste0(path,"prices.csv")))
      system(paste0(utilName,path))
    setwd(path)
    mat2 <- read.csv("prices.csv",
                     sep=',',header=F,stringsAsFactors=F)
    lab2 <- as.character(mat2[1,])
    prices2 <- apply(
      as.matrix(mat2[2:nrow(mat2),],nrow=(nrow(mat2)-1),ncol=ncol(mat2)),
      c(1,2),as.numeric)
    lab <- c(lab1,lab2)
    prices <- cbind(prices1,prices2)
  } else {
    lab <- lab1
    prices <- prices1
  }
  list(prices,lab)
}

findAllPrices <- function(dir,start=NA,end=NA,
            havePrices=F,needToAcquire=F,isSubDir=T) {
  isPlotInAdjCloses = FALSE
  createDirs(dir,isSubDir=isSubDir)
  res <- readSubDirs(dir,isSubDir=isSubDir)
  isCacheEnabled <- TRUE
  if(isSubDir) {
    D1  <- res[[1]]
    D2  <- res[[2]]
    lab <- res[[3]]
    D <- D1 + D2
  } else {
    D <- res[[1]]; lab <- res[[2]]
  }
  
  if(havePrices) { #was: havePrices
    print("have prices.")
    return(list(saveNPrices,saveNLab))
  } else {
    if(needToAcquire) {
      len <- length(get.hist.quote("A",quote="AdjClose",start,end))
      prices <- matrix(rep(NA,len*D),nrow=len,ncol=D)
      prices <- acquirePrices(prices,lab,len,D,D1,D2,
               start=start,end=end,dir,isSubDir=isSubDir)
    } else {
      print(dir)
      return(acquireCoalescedPrices(dir,isSubDir=isSubDir))
    }
  }
}

findBestSRLabs <- function(dir,prices=saveNPrices,threshSR,lab=saveNLab,
                         isOpt=TRUE,isSubDir=TRUE) {
  print(paste("findBestSRLabs dim(prices) =",nrow(prices),
              ncol(prices),"threshSR =",threshSR,"dir =",dir))
  print("calling elimSyms()")
  res    <- elimSyms(prices,lab,dir,isSubDir=isSubDir)
  print("returned from elimSyms()")
  prices <- res[[1]]
  lab    <- res[[2]]
  D      <- length(lab)
  D
  print(dim(prices))
  
  stopifnot(!is.na(prices[1,]) && prices>0.0)
  par(mfrow=c(1,1))
  plot(prices[nrow(prices),]/prices[1,],
       xlab="Security no.",
       main=paste("Gross Return: first price and last for",
                  nrow(prices),"days"))
  badPriceVecIdx <- which(prices[nrow(prices),]/prices[1,]<0)
  if(length(badPriceVecIdx) > 0) {
    print(paste("bad price vec idx =",badPriceVecIdx,"lab =",lab[badPriceVecIdx]))
    plot(prices[,badPriceVecIdx],type="l",col=2,main=lab[badPriceVecIdx])
  }
  
  R <- findR(prices)
  D <- dim(prices)[2]
  print(paste("D =",D))
  prices <<- prices
  R <<- R
  lab <<- lab
  
  res <- findCovMat(R)
  meanv    <- res[[1]]
  cov_mat  <- res[[2]]
  diag_cov_mat <- res[[3]]
  sdevv <- res[[4]]
  
  mufree <- 0
  pBStime <- system.time({
    res <- pruneBySharpe(prices,lab,meanv,sdevv,threshSR=threshSR)
  })[3]
  print(paste("pruneBySharpe time ====================> ",pBStime))
  prices   <- res[[1]]
  lab  <- res[[2]]
  sr   <- res[[3]]
  print(paste("dim(prices) =",dim(prices)))
  stopifnot(!is.na(prices[1,]))
  tryCatch({
    R   <- findR(prices)
  }, warning = function(w) {
    print(paste("try:",w))
  }, error = function(e) {
    print(paste("try:",e))
  })
  D   <- length(lab)
  print(paste('D =',D))
  
  res <- findCovMat(R)
  meanv    <- res[[1]]
  cov_mat  <- res[[2]]
  diag_cov_mat <- res[[3]]
  sdevv <- res[[4]]
  sdevv <- isnaCheckCovMat(R)
  #print(lab)
  print(dim(prices))
  if(F) checkDeterminant(prices,R,lab)
  isShorting <- FALSE
  daysPerYr <- 252
  print(paste("isOpt is",isOpt))
  if(isOpt) {
    library(quadprog)
    w <-opt(lab,meanv,cov_mat,isShorting)
    par(mfrow=c(1,1))
    maxw = max(w+.02)
    plot(w,ylim=c(0.01,maxw))
    text(w,lab,cex=.55,pos=3,col=4)
  } else
  {
    w <- rep(1/length(lab),length(lab))
  }
  t(cbind(lab[w > 0],w[w > 0]))
  displayCharts(prices[,w>0],lab[w>0],nrow=4,ncol=5)
  list(lab,w)
}

if(F) trendFollow3 <- function(lab,w) {
  fvec <- list.files(".", all.files = F)
  f <- fvec[length(fvec)]
  ############################### entry point:
  df <- read.csv(f)
  lab <- as.character(df[,2])
  w <- as.numeric(df[,3])
  return(list(lab,w))
}

load("/home/mark/FinAnalytics/RData.RData")
# dim(saveNPrices); length(saveNLab)
# prices <- saveNPrices; lab <- saveNLab
# saveNPrices <- prices; saveNLab <- lab

plotPruneAndPortPerf <- function(lab,start,end,tradeDays,
            w=rep(1/(length(lab)),length(lab)),ylim=c(.5,3.5)) {
  D <- length(lab)
  library(tseries)
  if(w[1] - 1/D < .001) { print("equal weighting"); w <- rep(1/D,D) }
  pricesD <- getHistPrices(lab,w,tradeDays,
                           start=start,end=end)
  dim(pricesD)
  print(pricesD[1,]);print(lab)
  #Eliminate any non-priceable tickers:
  lab     <- lab[!is.na(pricesD[1,])]
  w       <- w[!is.na(pricesD[1,])]
  pricesD <- matrix(pricesD[,!is.na(pricesD[1,])],nrow=nrow(pricesD))
  amtToRealloc <- 1-sum(w)
  wInc <- w/sum(w)*amtToRealloc
  w <- w+wInc
  D <- length(lab)
  print("Pruned NA price vectors")
  print(paste("w =",w))
  #Final lab now:
  for(i in 1) plot(pricesD[,i],type="l",
         main=lab[i],xlab="days",ylab="USD")
  #Use fn. from book Section 4.8:
  plotMultSeries(pricesD,lab,w,D,cc=NA,ret=NA,
                 ylim=ylim)
  D <- ncol(pricesD)
  len <- nrow(pricesD)
  #Now add the middle line for the total port. value:
  normalizedPrices <- (pricesD[1:len,1:D]/
          t(matrix(rep(pricesD[1:1,1:D],len),nrow=D)))
  portGrossRet <- normalizedPrices %*% t(t(w))
  lines(portGrossRet,type="l",lwd=3)
  portGrossRet[len] #Gross return
}

#MVO3:
len <- length(get.hist.quote("SON",start=start,end=end,quote="AdjClose"))
if(F) for(i in 1:length(lab))
  tryCatch({
    plotPruneAndPortPerf(lab[i],start=start,end=end,tradeDays=len,ylim=c(.3,3))
  }, error = function(e) {
    print(paste(e,i,"****************",lab[i]))
  })
#MVO3 Last quarter:
if(F) {
grossRet <- plotPruneAndPortPerf(lab,start=start,end=end,
                                 tradeDays=len,ylim=c(.65,2.3))  #equal w
grossRet
spPrices <- getHistPrices(c("^GSPC"),c(1),len,start=start,end=end)
spGrossRet <- spPrices[length(spPrices)]/spPrices[1]
spGrossRet
(grossRet-1)/(spGrossRet-1)
lines(spPrices/spPrices[1],col=5,lwd=3)
}

reportGrossRet <- function(lab,w,start,end) {
  print(paste("attempting to find vectorlen.",start,end))
  stopifnot(start<end)
  tryCatch({
    print(paste(start,end))
    len <- length(get.hist.quote("A",start=start,end=end,quote="AdjClose"))
  }, warning = function(w) { print(w); stop()
  }, error = function(e) { print(e); stop() })
  print(paste("len =",len))
  if(F) grossRet <- plotPruneAndPortPerf(lab,start=start,end=end,
                        tradeDays=len,w=w,ylim=c(.5,2.4))  #equal w
  if(F) print(paste("grossRet w",grossRet))
  print(paste("length(lab) =",length(lab))) 
  grossRet <- plotPruneAndPortPerf(lab,start=start,end=end,
                        tradeDays=len,ylim=c(.50,1.75))  #equal w
  print(paste("grossRet w=1/D",grossRet,"from",start,"to",end))
  spPrices <- getHistPrices(c("^GSPC"),c(1),len,start=start,end=end)
  spGrossRet <- spPrices[length(spPrices)]/spPrices[1]
  print(paste("spGrossRet =",spGrossRet,"from",start,"to",end))
  print((grossRet-1)/(spGrossRet-1))
  list(grossRet,spGrossRet,lab,w)
}

momemtumBackTest <- function(dir,threshSR=6,start,end,
                        startOOS,endOOS,isSubDir=TRUE) {
  print(paste("momemtumBackTest threshSR =",threshSR,dir))
  res <- findAllPrices(dir,isSubDir=isSubDir)
  prices <- res[[1]]; lab <- res[[2]]
  saveNPrices <<- prices; saveNLab <<- lab
  res <- findBestSRLabs(dir,prices,lab,isOpt=F,
                        threshSR=threshSR,isSubDir=isSubDir)
  if(F) res <- trendFollow3()
  lab <- res[[1]]; w <- res[[2]]
  reportGrossRet(lab,w,startOOS,endOOS)
}

writeWeights <- function(dir,lab,w) {
  numNonZeroWs = sum(ceiling(w))
  QPtype <- 1
  setwd(paste(homeuser,"/FinAnalytics/",dir,"/",sep=""))
  fileName = paste("resD",numNonZeroWs,"EW",toString(QPtype),
                   "Days",len,".csv",sep="")
  if(file.exists(fileName))
    stop(paste(getwd(),fileName,"already exists"))
  contents = cbind(lab,w)
  o <- order(-w)
  write.csv(contents[o,][1:numNonZeroWs,],file=fileName)
}

#START #######################################################
load("/home/mark/FinAnalytics/RData.RData")
library(tseries)
ew = vector(length=46); sp = vector(length=46)
homeuser="/home/mark"
csvfn <- paste0(homeuser,"/FinAnalytics/*/res*.csv")
if(exists(csvfn))
  system(paste0("rm ",csvfn))
#par(mfrow=c(2,2))
threshSR <- 19
  
retIdx <- 0
threshSR <- 19
dir <- "MVO3.2019.02"
res <- momemtumBackTest(
  dir = dir, threshSR = threshSR,
  start = "2016-02-22",
  end   = "2019-02-21",
  startOOS = "2019-02-22",
  endOOS   = "2019-03-21")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])

threshSR <- 26
dir <- "MVO3.2018.11"
res <- momemtumBackTest(
  dir = dir, threshSR = threshSR,
  start = "2015-11-22",
  end   = "2018-11-21",
  startOOS = "2018-11-22",
  endOOS   = "2018-12-07")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])


threshSR <- 19
dir <- "MVO3.2018.08"
res <- momemtumBackTest(
  dir = dir, threshSR = threshSR,
  start = "2015-08-22",
  end   = "2018-08-24",
  startOOS = "2018-08-21",
  endOOS   = "2018-11-20")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])

if(F){
quoteLab <- sort(res[[3]])
quotePrices <- sapply(quoteLab, get.hist.quote, start="2018-08-22",
      end="2018-08-22",quote="AdjClose")
quotePrices
526099/quotePrices/19
}

dir <- "MVO3.2018.05"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2015-05-21",
end   = "2018-05-20",
startOOS = "2018-05-21",
endOOS   = "2018-08-20")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])
  
dir <- "MVO3.2018.02"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2015-02-21",
end   = "2018-02-20",
startOOS = "2018-02-21",
endOOS   = "2018-05-20")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2017.11"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2014-11-21",
end   = "2017-11-20",
startOOS = "2017-11-21",
endOOS   = "2018-02-20")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2017.08"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2014-08-29",
end   = "2017-08-29",
startOOS = "2017-08-29",
endOOS   = "2017-11-29")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2017.05"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2014-05-21",
end   = "2017-05-20",
startOOS = "2017-05-21",
endOOS   = "2017-08-20")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2017.02"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2014-02-21",
end   = "2017-02-20",
startOOS = "2017-02-21",
endOOS   = "2017-05-20")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
dxwriteWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2016.11"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2013-11-21",
end   = "2016-11-20",
startOOS = "2016-11-20",
endOOS   = "2017-02-20")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2016.08"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2013-08-21",
end   = "2016-08-20",
startOOS = "2016-08-20",
endOOS   = "2016-11-20")
retIdx<-retIdx+1; ew[retIdx] <- res[[1]]; sp[retIdx] <- res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2016.05"
res <- momemtumBackTest(
dir = dir, threshSR = threshSR,
start = "2013-05-21",
end   = "2016-05-20",
startOOS = "2016-05-20", 
endOOS   = "2016-08-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2016.02"
res <- momemtumBackTest(
  dir = dir, threshSR = threshSR,
  start = "2013-02-21",
  end   = "2016-02-20",
  startOOS = "2016-02-20", 
  endOOS   = "2016-05-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2015.11"
res <- momemtumBackTest(
  dir = dir, threshSR = threshSR,
  start = "2012-11-21",
  end   = "2015-11-20",
  startOOS = "2015-11-20", 
  endOOS   = "2016-02-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2015.08"
res <- momemtumBackTest(
  dir = dir, threshSR = threshSR,
  start = "2012-08-21",
  end   = "2015-08-20",
  startOOS = "2015-08-20", 
  endOOS   = "2015-11-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2015.05"
res <- momemtumBackTest(
  dir   = dir,
  start = "2012-05-21",
  end   = "2015-05-20",
  startOOS = "2015-05-20", 
  endOOS   = "2015-08-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2015.02"
res <- momemtumBackTest(
  dir   = dir,
  start = "2012-02-21",
  end   = "2015-02-20",
  startOOS = "2015-02-20", 
  endOOS   = "2015-05-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2014.11"
res <- momemtumBackTest(
  dir   = dir,
  start = "2011-11-21",
  end   = "2014-11-20",
  startOOS = "2014-11-20", 
  endOOS   = "2015-02-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <-"MVO3.2014.08"
res <- momemtumBackTest(
  dir   = dir,
  start = "2011-08-21",
  end   = "2014-08-20",
  startOOS = "2014-08-20", 
  endOOS   = "2014-11-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2014.05"
res <- momemtumBackTest(
  dir   = dir,
  start = "2011-05-21",
  end   = "2014-05-20",
  startOOS = "2014-05-20", 
  endOOS   = "2014-08-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2014.02"
res <- momemtumBackTest(
  dir   = dir,
  start = "2011-02-21",
  end   = "2014-02-20",
  startOOS = "2014-02-20", 
  endOOS   = "2014-05-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2013.11"
res <- momemtumBackTest(
  dir   = dir,
  start = "2010-11-21",
  end   = "2013-11-20",
  startOOS = "2013-11-20", 
  endOOS   = "2014-02-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2013.08"
res <- momemtumBackTest(
  dir   = dir,
  start = "2010-08-21",
  end   = "2013-08-20",
  startOOS = "2013-08-20", 
  endOOS   = "2013-11-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2013.05"
res <- momemtumBackTest(
  dir   = dir,
  start = "2010-05-21",
  end   = "2013-05-20",
  startOOS = "2013-05-20", 
  endOOS   = "2013-08-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2013.02"
res <- momemtumBackTest(
  dir   = dir,
  start = "2010-02-21",
  end   = "2013-02-20",
  startOOS = "2013-02-20", 
  endOOS   = "2013-05-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2012.11"
res <- momemtumBackTest(
  dir   = dir,
  start = "2009-11-21",
  end   = "2012-11-20",
  startOOS = "2012-11-20", 
  endOOS   = "2013-02-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

dir <- "MVO3.2012.08"
res <- momemtumBackTest(
  dir   = dir,
  start = "2009-08-21",
  end   = "2012-08-20",
  startOOS = "2012-08-20", 
  endOOS   = "2012-11-20")
retIdx<-retIdx+1; ew[retIdx] = res[[1]]; sp[retIdx] = res[[2]]
writeWeights(dir,res[[3]],res[[4]])

#prod(mv)
prod(ew); prod(sp)
#spmv <- cumprod(rev(mv))
spew <- cumprod(rev(ew[1:retIdx]))
spsp <- cumprod(rev(sp[1:retIdx]))
spew
spsp
#Compute SR of of each:
ewLogRet <- diff(log(spew))
mean(ewLogRet)/sd(ewLogRet)*sqrt(4)
spLogRet <- diff(log(spsp))
mean(spLogRet)/sd(spLogRet)*sqrt(4)
plot(0:(retIdx),c(1,spew[1:retIdx]),type="l",col=4,ylim=c(.8,3.5),
  main=paste(retIdx,"qtr. perf. 2013-02-21 on p =",
             threshSR),xlab="qtr")
lines(0:(retIdx),c(1,spsp[1:retIdx]),type="l",col=3)
spew[length(spew)]-spsp[length(spsp)]
stop("Normal exit.")

######################################################
# Only for gathering non-existing prices:

rm(list=ls())
library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2019.02",
                     start = "2016-02-22",
                     end   = "2019-02-21",needToAcquire=T)

load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2018.11",
                    start = "2015-11-22",
                    end   = "2018-11-21",needToAcquire=T)

load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2018.08",
                    start = "2015-08-21",
                    end   = "2018-08-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2018.05",
                    start = "2015-05-21",
                    end   = "2018-05-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2018.02",
start = "2015-02-21",
end   = "2018-02-20",needToAcquire=T)

library(tseries)
res <- findAllPrices(dir = "MVO3.2016.02",
             start = "2013-02-21",
             end   = "2016-02-20")

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2015.11",
                    start = "2012-11-21",
                    end   = "2015-11-20")

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2015.08",
                    start = "2012-08-21",
                    end   = "2015-08-20")

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2015.05",
                    start = "2012-05-21",
                    end   = "2015-05-20")

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2015.02",
                    start = "2012-02-21",
                    end   = "2015-02-20")

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2014.11",
                    start = "2011-11-21",
                    end   = "2014-11-20")

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2014.08",
                    start = "2011-08-21",
                    end   = "2014-08-20")

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2014.05",
                    start = "2011-05-21",
                    end   = "2014-05-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2014.02",
                    start = "2011-02-21",
                    end   = "2014-02-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2013.11",
                    start = "2010-11-21",
                    end   = "2013-11-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2013.08",
                    start = "2010-08-21",
                    end   = "2013-08-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2013.05",
                    start = "2010-05-21",
                    end   = "2013-05-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2013.02",
                    start = "2010-02-21",
                    end   = "2013-02-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2012.11",
                    start = "2009-11-21",
                    end   = "2012-11-20",needToAcquire=T)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "MVO3.2012.08",
                    start = "2009-08-21",
                    end   = "2012-08-20",needToAcquire=T)

#Investor's Business Daily published Top-10 One Month Winners Jan 21 2019
library(tseries)
ETFleaders<-c("EIDO","EPHE","PUI","EWZ","ITM","TFI","FBZ","BSCK",
              "MNA","SHM","VGSH","BSV","SCHO","SHY","GVI","MBB","AGG",
              "BIV","SUB","BND","VMBS")
#Investor's Business Daily published ETFleaders Jan 21 2019
ETFleaders <- c("XES","USO","XOP","EWZ","FBT","FCG","ILF","FXN","OIH","DBO")
ETFleaders <- c("ADM","COP","CVX","INGR","NHF","NXRT","TARO","TPL")
D <- length(ETFleaders)
w <- rep(1/D,D)
start <- "1985-01-01"
start <- "2002-01-21"
#start <- "2018-01-21"
end   <- "2019-01-20"
#start <- "2017-05-21"
#end   <- "2018-05-20"
#start <- "2012-05-21"
#end   <- "2017-05-20"
qqqpv <- get.hist.quote("QQQ",start=start,end=end,quote="AdjClose")
tradeDays <- length(qqqpv)
Rqqq <- diff(log(qqqpv))*100
SRqqq <- mean(Rqqq)/sd(Rqqq)
print(SRqqq*sqrt(252))

ETFleaders <- c("QQQ","BRK-B","IJT","^GSPC")
par(mfrow=c(2,2))
for(l in ETFleaders)
  plot(1:tradeDays,
    get.hist.quote(l,start=start,end=end,quote="AdjClose")[,1],
    type="l",xlab="",ylab="",main=l)
prices <- getHistPrices(ETFleaders,w,tradeDays,
                         start=start,end=end)
prices[1,]
plotMultSeries(prices,ETFleaders,w=rep(1/4,4),D=4,ylim=c(.5,6))
#prices <- prices[,-c(7:8)]
#ETFleaders <- ETFleaders[-c(7:8)]
D <- length(ETFleaders)

plotMultSeries(prices,ETFleaders,w,D,cc="days",ret=NA,
         ylim=c(.5,5.5),isAlone=TRUE)
spPrices <- getHistPrices(c("^GSPC"),c(1),#c("^GSPC"),c(1),
                          tradeDays,start=start,end=end)
Rsp <- diff(log(spPrices))*100
SRsp <- mean(Rsp)/sd(Rsp)
print(SRsp*sqrt(252))
spGrossRet <- spPrices[length(spPrices)]/spPrices[1]
spGrossRet
lines(spPrices/spPrices[1],col=1,lwd=3)

library(tseries)
load("/home/mark/FinAnalytics/RData.RData")
res <- findAllPrices(dir = "ETF",
                     start = "2015-12-03",
                     end   = "2017-11-30",needToAcquire=FALSE,isSubDir=FALSE)
dir <- "ETF"
threshSR <- 20
res <- momemtumBackTest(
  dir = dir, threshSR = threshSR,
  start = "2015-12-03",
  end   = "2017-11-30",
  startOOS = "2017-11-30", 
  endOOS   = "2018-11-30",isSubDir=FALSE)
