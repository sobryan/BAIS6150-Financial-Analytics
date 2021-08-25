#Chapter IV
1000/(1.02)^30
1000/(1.03)^30

P<-1000
T<-20
r<-.06
C<-30

BV <- function(P,C,r,t,T) {
  #Finds coupon Bond Value at time t mat T
  tmat <- T-t
  accrued <- C*2*t #already paid
  if(tmat != 0) { #include interim coupons
    i <- seq(1,2*tmat)
    accrued + sum(C/(1+r/2)^i) +
      P/(1+r/2)^(2*tmat)
  } else #no coupons left
    accrued + P/(1+r/2)^(2*tmat)
}

set.seed(437)
par(mfrow=c(1,3))
#Simulate rates market for r
rvec <- round(c(r,r+
              rnorm(T)*.0050),4)
plot(rvec,type="l",ylim=c(0,.07),
     xlab="Years",ylab="r",col=4)
points(rvec,col=4)
#Simulate PV of Bond at time t
simBV <- function(P,C,rvec,T) {
  BVvec = rep(0,T)
  for(t in 0:T) {
    i = t+1
    BVvec[i] <- BV(P,C,rvec[i],t,T)
  }
  plot(BVvec,type="l",col=4,ylim=c(0,2500),
       xlab="Years",ylab="Bond Value")
  points(BVvec,col=4)
  BVvec
}
BV(P,C,r=.06,t=0,T=20)
BV(P,C,r=.06,t=1/2,T=20)
BV(P,C,r=.06,t=1,T=20)
BV(P,C,r=.07,t=1/2,T=20)
BV(P,C,r=.06,t=20,T=20)
BV(P,C,r=0,t=0,T=20)

C <- 0
simBV(P,C,rvec,T)
C <- 30
simBV(P,C,rvec,T)

#Stock position:
par(mfrow=c(1,1))
#Simulate rates market for r
T <- 45 #days not years
Svec <- round(c(1,1+1.1*
              rnorm(T)*.0025),4)
SVvec <- 10000*Svec
plot(SVvec,type="l",col=4,ylim=c(9800,10200),
     xlab="Days",ylab="Stock Value")
points(SVvec,col=4)
text(c(1,T),c(10050,10050),c("S(0)","S(T)"))

displayCharts <- function(prices,lab,nrow=3,ncol=4,sleepSecs=4) {
  Dims=length(prices[1,])
  for(chartGrp in (1:ceiling(Dims/(nrow*ncol)))) {
    print(chartGrp)
    par(mar=c(3.82,1.82,1.82,0.82))
    par(mfrow=c(nrow,ncol))
    for(i in 1:(nrow*ncol)) {
      j = ((chartGrp-1)*nrow*ncol+i)
      if(j <= Dims) {
        print(paste(j,lab[j]))
        plot(prices[,j],type="l",xlab=paste(j,lab[j]))
      }
    }
    Sys.sleep(sleepSecs)
  }
}
#unit test
prices <- matrix(rep(1,10),nrow=5,ncol=2)
prices[3,] <- c(6,6)
prices[4,2] <- 2
lab <- c("X","Y")
displayCharts(prices,lab)

library(huge)
data(stockdata)
D = length(stockdata$data[1,])
len = length(stockdata$data[,1])
prices = stockdata$data[,1:D]
lab = stockdata$info[1:D,1]
displayCharts(prices[,1:12],lab[1:12],sleepSec=0)

splitAdjust <- function(prices,symbol) {
  len = length(prices)
  origFinalPrice = prices[len]
  for(j in 2:len) {
    split = 0
    #print(paste(prices[j-1],prices[j]))
    if(prices[j-1] >= 1.4*prices[j]) {
      split = +1.5 # a 3 for 2
      if(prices[j-1] >= 1.8*prices[j])
        split = +2 #At least a 2 for 1
      if(prices[j-1] >= 2.9*prices[j])
        split = +3 #Ah a 3 for 1
      if(prices[j-1] >= 3.9*prices[j])
        split = +4 #Ah a 3 for 1
      if(prices[j-1] >= 4.9*prices[j])
        stop(paste(symbol,'detected more than 4:1 split'))
      print(paste("split adjusting",symbol,split,
              j,prices[j-1],prices[j]))
    } #reverse splits: price increases so divide
    if(prices[j-1] <= prices[j]/1.4) {
      split = -1.5
      if(prices[j-1] <= prices[j]/1.9 &&
           prices[j-1] >= prices[j]/2.1)
      split = -2
      if(prices[j-1] <= prices[j]/2.9 &&
           prices[j-1] >= prices[j]/3.1)
        split = -3
      if(prices[j-1] <= prices[j]/5.8 &&
           prices[j-1] >= prices[j]/6.2)
        split = -6
      if((prices[j-1] <= prices[j]/7.7) &&
           (prices[j-1] >= prices[j]/8.3))
        split = -8
      if((prices[j-1] <= prices[j]/9.7) &&
           (prices[j-1] >= prices[j]/10.3))
        split = -10
      if((split == 0) && (prices[j-1] <= prices[j]/2.9))
        stop(paste(symbol,
                   'detected more than double reverse split'))
      print(paste("reverse split adjusting",j,symbol,j,
                  split,prices[j-1],prices[j]))
    }
    if(split != 0) {
      for(k in j:len) { #adjust all prices to right from j:len
        if(symbol=="C")
          prices[k] = prices[k]/10 #hard coded for Citi
        else if(split == +1.5)
          prices[k] = 1.5*prices[k] # 3 for 2
        else if(split == +2)
          prices[k] = 2*prices[k] # 2 to 1
        else if(split == +3)
          prices[k] = 3*prices[k] # 3 to 1
        else if(split == +4)
          prices[k] = 4*prices[k] # 4 to 1
        else if(split == -1.5)
          prices[k] = prices[k]/1.5 # 2 to 3 rev
        else if(split == -2)
          prices[k] = prices[k]/2 # 1 to 2 rev
        else if(split == -3)
          prices[k] = prices[k]/3 # 1 to 2 rev
        else if(split == -6)
          prices[k] = prices[k]/6 # 1 to 8 rev
        else if(split == -8)
          prices[k] = prices[k]/8 # 1 to 8 rev
        else if(split == -10)
          prices[k] = prices[k]/10 # 1 to 10 rev
        else stop('splitAdjust internal error')
      }
    }
  }
  finalPrice = prices[len]
  return(prices*origFinalPrice/finalPrice)
}
#unit test:
p <- c(3.0,3.0,2.0,11.88,5.9,1.95,3.90,3.90,
       1.5,.75,1.00,1.2,1.4,1.8,2.1,1.05,
       1.30,1.31,1.32,.44,.43,.11,.12,.13)
sap <- splitAdjust(p,"SYM")
paste(p, collapse=',')
paste(round(sap,3), collapse=',')
plot(p,type='l',ylim=c(0,15)); points(sap,col=4)

JDSUidx <- match('JDSU',lab); par(mfrow=c(1,1))
plot(prices[,JDSUidx],type='l',xlab='JDSU')
adjp<-splitAdjust(prices[,JDSUidx],c('JDSU')); par(mfrow=c(1,1))
plot(adjp,type='l',xlab='JDSUadj')

findR <- function(prices,isSplitAdjusted=TRUE) {#Find R: logrets:
  len <- dim(prices)[1]
  D   <<- dim(prices)[2]
  R   <- matrix(nrow=(len-1),ncol=D)
  for(i in 1:D) {
    #print(i)
    if(!isSplitAdjusted) prices[,i] <<- splitAdjust(prices[,i],lab[i])
    R[,i] = 100*diff(log(prices[,i])) ###log rets
  }
  R
}

R <- findR(prices,isSplitAdjusted=FALSE)
D <- dim(prices)[2]
D

adjustForMergers <- function(dir,portFile) {
  #Take in symbols and their weights and emit a
  #rebalanced file summing close to 1.0
  setwd(paste(homeuser,"/FinAnalytics/",dir,"/",sep=""))
  df <- read.csv(portFile)
  lab <- df[,2]
  w <- df[,3]
  if(abs(sum(w) - 1.0) < .002) {
    print('All weights sum to 1.')
  } else {
    print(sum(w))
    amtToRealloc <- 1.0 - sum(w)
    wInc <- w/sum(w)*amtToRealloc
    print(sum(w+wInc))
    df[,3] <- w+wInc
    newFile = paste("rebal",portFile,sep="")
    write.csv(df,file=newFile,row.names = FALSE)
    print(paste("wrote file",newFile))
  }
}
adjustForMergers('huge','resD26QP1Days1258.csv')
adjustForMergers('huge','resD25Days1258woTIE.csv')
adjustForMergers('huge','resD24Days1258.csv')

plotMultSeries <- function(prices,lab,w,D,cc="days",ret=NA,
                           ylim=c(.2,15),isAlone=TRUE) {
  if(isAlone) plot.new()
  mapToCol <- function(d)
    if(d%%8==7) 1 else if(d==8)
      2 else if(d==15) 3 else if(d==23) 4 else d
  par(mar=c(4,2.82,1.82,1))
  if(isAlone) par(mfrow=c(1,1))
  tot <- 0; len <- dim(prices)[1]
  first <- TRUE; D <- dim(prices)[2]
  for(d in 1:D) {
    if(!is.na(prices[1,d]) && !is.na(w[d]) && w[d] > 0) {
      print(lab[d])
      tot <- tot + 1
      if(first) {
        first = FALSE
        plot(prices[,d]/prices[1,d],type="l",
             col=mapToCol(d),xlab=cc,
             ylim=ylim)
      } else
        lines(prices[,d]/prices[1,d],type="l",
              col=mapToCol(d))
      text(len,(prices[len,d]/prices[1,d]),lab[d],
           col=mapToCol(d),cex=.8)
    }
  }
  print(tot)
  print(paste("density or non-zero weights (sparsity) is ",tot/D))
}
#unit test:
D2 <- 12
w <- rep(1/D2,D2)
plotMultSeries(prices,lab,w,D2,cc=
  paste(sum(w>0),"stocks"),ret="", ylim=c(.5,8))

library(tseries)
pv <- get.hist.quote('AAPL',quote="Adj",start="2011-02-09",
                     end="2015-02-09")
pv

readExchSymbols <- function(fileName) {
  frame <- read.csv(fileName,header=TRUE,sep="\t")
  return(as.character(frame[,1]))
}

createDirs <- function(dir,isSubDir=TRUE) {
  #check for the two subdirs if isSubDir TRUE
  mainDir <- paste(homeuser,"/FinAnalytics/",sep="")
  destDir <- paste(mainDir,dir,sep="")
  if (!file.exists(destDir))
    dir.create(file.path(destDir))
  setwd(file.path(destDir))
  if(isSubDir) {
    f1 <- "NYSEclean.txt"
    f2 <- "NASDAQclean.txt"
  
    NYSEsubDir <- paste(destDir,"/NYSE",sep="")
    if (!file.exists(NYSEsubDir))
      dir.create(file.path(NYSEsubDir))
    if(!file.exists(paste(NYSEsubDir,"/NYSEclean.txt",sep="")))
      file.copy(paste0(homeuser,"/FinAnalytics/",f1),
             NYSEsubDir)
    
    NASDAQsubDir <- paste(destDir,"/NASDAQ",sep="")
    if (!file.exists(NASDAQsubDir))
      dir.create(file.path(NASDAQsubDir))
    if(!file.exists(paste(NASDAQsubDir,"/NASDAQclean.txt",sep="")))
      file.copy(paste0(homeuser,"/FinAnalytics/",f2),
             NASDAQsubDir)
  } else {
    f <- paste(dir,"clean.txt",sep="")
    if(!file.exists(paste(destDir,"/",f,sep="")))
      if(file.exists(paste(mainDir,"/",f,sep="")))
        file.copy(paste0(homeuser,"/FinAnalytics/",f),".")
  }
}
#unit test
createDirs("CDUT")

readSubDirs <- function(dir,isSubDir=TRUE) {
  if(isSubDir) {
    #Case: 2 sub-dirs: NYSE and NASDAQ
    #Return 3 results, the last being a large vec
    setwd(paste(homeuser,"/FinAnalytics/",dir,"/NYSE",sep=""))
    lab <- readExchSymbols("NYSEclean.txt")
    D1 <- length(lab)
    print(D1)
    setwd(paste(homeuser,"/FinAnalytics/",dir,"/NASDAQ",sep=""))
    lab2 <- readExchSymbols("NASDAQclean.txt")
    lab <- append(lab,lab2)
    D2 <- length(lab2)
    print(D2)
    list(D1,D2,as.character(lab))
  } else {
    setwd(paste(homeuser,"/FinAnalytics/",dir,sep=""))
    lab <- readExchSymbols(paste(dir,"clean.txt",sep=""))
    D <- length(lab)
    print(D)
    list(D,as.character(lab))
  }
}

acquirePrices <- function(prices,lab,len,D,D1,D2,dir,
                 start,end,isSubDir=TRUE,verbose=TRUE) {
  isSuccessfulQuote <- FALSE
  for(d in 1:D) {
    if(d == 1 || (isSubDir && d == (D1+1)))
      if(d == 1 && isSubDir) {
        setwd(paste(homeuser,"/FinAnalytics/",dir,"/NYSE",sep=""))
        unlink('bad*')
        print(paste("NYSE=======:",d))
      } else if(d == (D1+1) && isSubDir) {
        setwd(paste(homeuser,"/FinAnalytics/",dir,"/NASDAQ",sep=""))
        unlink('bad*')
        print(paste("NASDAQ=======:",d))
      } else {
        setwd(paste(homeuser,"/FinAnalytics/",dir,sep=""))
        unlink('bad*')
        print(paste("ETF==========:",d))
      }
    if(verbose) print(paste(d,lab[d]))
    fileName = paste("cached",lab[d],".csv",sep="")
    usingCacheThisFileName <- FALSE
    if(file.exists(fileName)) {
      usingCacheThisFileName <- TRUE
      pricesForStock <- read.csv(fileName,header=TRUE,sep="")[,1]
      if(!is.na(pricesForStock[1]))
        isSuccessfulQuote <- TRUE
    }
    if(!usingCacheThisFileName ||
         (usingCacheThisFileName && length(pricesForStock) != len)) {
      usingCacheThisFileName <- FALSE
      tryCatch( {
        #print(start);print(end)
        Sys.sleep(1)
        pricesForStock <- get.hist.quote(lab[d],quote="Adj",
                                         start=start,end=end)
        if(!is.na(pricesForStock[1]))
          isSuccessfulQuote <- TRUE
      }, error = function(err) {
        print(err);cat(lab[d],file="badsyms.txt",
                       append=TRUE,sep="\n")
        isSuccessfulQuote <- FALSE
      } )
    }
    if(length(pricesForStock) == len) {
      prices[,d] <- pricesForStock
      if(sum(is.na(prices[,d])) > 0 || (sum(is.na(prices[,d-1])) == 0 &&
            d > 1 && prices[1,d] == prices[1,d-1])) {
        print(paste(lab[d],"has NA prices"))
        cat(lab[d],file="badsyms.txt",
            append=TRUE,sep="\n")
        isSuccessfulQuote <- FALSE
      }
    } else {
      cat(lab[d],file="badsyms.txt",append=TRUE,sep="\n")
    }
    if(!isSuccessfulQuote)
      cat(lab[d],file="badsyms.txt",append=TRUE,sep="\n")
    if(isPlotInAdjCloses) {
      if(d == 1)
        plot(prices[,d]/prices[1,d],type="l",col="blue",ylim=c(.2,6))
      else
        lines(prices[,d]/prices[1,d],type="l",col="blue")
      text(len,(prices[len,d]/prices[1,d]),lab[d],cex=.6)
    }
    if(isCacheEnabled && !usingCacheThisFileName &&
         isSuccessfulQuote) {
      #save redundant re-write
      fileName = paste("cached",lab[d],".csv",sep="")
      print(fileName)
      write.csv(prices[,d],file=fileName,row.names = FALSE)
    }
    isSplitAdjusted = TRUE
  }
  prices
}

library(tseries)
APUT <- function(isTestElimSyms=FALSE) {
  dir <- 'APUT'
  l1 <- c('A','AA','AAN','AAP','AAT','AAV','AB','ABB','ABC','ABG',
          'ABM','ABR','ABX','ACC','ACCO','ACE','ACG','ACH','ACI','ACM')
  l2 <- c('AAL','AAME','AAON','AAPL','AAWW','AAXJ','ABAX','ABCB',
          'ABCD','ABCO','ABIO','ABMD','ABTL','ACAD','ACAS',
          'ACAT','ACCL','ACET','ACFC','ACFN','ACGL','ACHC','ACHN',
          'ACIW','ACLS')
  topdir <- paste(homeuser,'/FinAnalytics/',dir,sep="")
  NYSEdir <- paste(topdir,'/NYSE',sep="")
  NASDAQdir <- paste(topdir,'/NASDAQ',sep="")
  if(!file.exists(topdir))
    dir.create(topdir)
  if(!file.exists(NYSEdir)) {
    dir.create(NYSEdir)
    setwd(NYSEdir)
    if(!file.exists("NYSEclean.txt"))
      write.csv(l1,file="NYSEclean.txt",
                quote=FALSE,row.names=FALSE)
  }
  if(!file.exists(NASDAQdir)) {
    dir.create(NASDAQdir)
    setwd(NASDAQdir)
    if(!file.exists("NASDAQclean.txt"))
      write.csv(l2,file="NASDAQclean.txt",
                quote=FALSE,row.names=FALSE)
  }
  D1 <- length(l1)
  D2 <- length(l2)
  l <- c(l1,l2)
  D <- D1 + D2
  len <- length(get.hist.quote("NVDA",start="2010-02-17",end="2014-02-14",quote="AdjClose"))
  p <- matrix(rep(NA,len*D),nrow=len,ncol=D)
  #acquirePrices assumes user knows proper
  #len, start and end
  isPlotInAdjCloses <<- FALSE
  isCacheEnabled <<- TRUE
  p <- acquirePrices(p,l,len,D,D1,D2,dir,
       start="2010-02-17",end="2014-02-14",isSubDir=TRUE)
  #Second time cached files exist.
  p <- acquirePrices(p,l,len,D,D1,D2,dir,
       start="2010-02-17",end="2014-02-14",isSubDir=TRUE)
  if(isTestElimSyms) {
    dim(p)
    D
    system(paste('sort ',paste(NYSEdir,'/bad*',sep="")))
    system(paste('sort ',paste(NASDAQdir,'/bad*',sep="")))
    saveD <- D
    res <- elimSyms(p,l,"APUT")
    p <- res[[1]]
    l <- res[[2]]
    print(paste("elimSyms returns",l))
    #print(p[1,])
  }
  unlink(topdir, recursive = TRUE)
  p[len,]
}
#acquirePrices unit test (APUT):
APUT()

elimSyms <- function(prices,lab,dir,isSubDir=TRUE) {
  len = dim(prices)[1]
  D = dim(prices)[2]
  #First find removal list in 3 files in each of NYSE and NASDAQ
  indInFile = as.vector(rep(FALSE,D))
  ifelse(isSubDir,subdirVec <- c("NYSE","NASDAQ"),subdirVec <- c(NA))
  for(subdir in subdirVec) {
    if(isSubDir)
      setwd(paste(homeuser,"/FinAnalytics/",dir,"/",subdir,sep=""))
    else
      setwd(paste(homeuser,"/FinAnalytics/",dir,sep=""))
    for(file in c("badsyms.txt","badcors.txt","badsharpes.txt")) {
      badlab = NA
      if(file.exists(file))
        badlab <- read.table(file) # badcors.txt badsharpes.txt")
      if(length(badlab)>1 || !is.na(badlab)) {
        for(l in badlab) {
          print(paste("elimSym",l))
          pos = match(l,lab)
          indInFile[pos] = TRUE
        }
      }
    }
  }
  indNAPrices = (is.na(prices[1,]))
  indNALab = (is.na(lab[1:D]))
  indTooBig = (prices[1,] > 1e5) | (prices[len,] > 1e5)
  #missing price or lab is NA or too big
  indUnion = indInFile | indNAPrices | indNALab | indTooBig
  #Create new prices matrix smaller for only NonNAs
  smallerSz = D - sum(indUnion)
  print(smallerSz)
  newPrices = matrix(rep(0,len*smallerSz),nrow=len,ncol=smallerSz)
  newLab = vector(length=smallerSz)
  e <- 1
  for(d in 1:D) {
    if(!indUnion[d]) {
      #print(paste("e",e,lab))
      newPrices[,e] <- prices[,d]
      newLab[e] <- lab[d]
      e <- e + 1
    } else {print(d)}
  }
  list(newPrices[,1:smallerSz],newLab)
}
#unit test:
APUT(TRUE)

isPlotInAdjCloses = FALSE
dir <- 'MVO4'
len <- 1006
createDirs(dir)
res <- readSubDirs(dir)
isCacheEnabled <- TRUE
D1  <- res[[1]]
D2  <- res[[2]]
lab <- res[[3]]
D <- D1 + D2
start = "2011-02-09"
end   = "2015-02-09"

prices <- matrix(rep(NA,len*D),nrow=len,ncol=D)
prices <- acquirePrices(prices,lab,len,D,D1,D2,
                        start=start,end=end,dir,isSubDir=TRUE)

res    <- elimSyms(prices,lab,dir,isSubDir=TRUE)
prices <- res[[1]]
lab    <- res[[2]]
D      <- length(lab)
D
dim(prices)
R <- findR(prices)
D <- dim(prices)[2]

findCovMat <- function(R) {
  meanv <- apply(R,2,mean)
  cov_mat <- cov(R)
  diag_cov_mat <- diag(cov_mat)
  sdevv <- sqrt(diag(cov_mat))
  list(meanv,cov_mat,diag_cov_mat,sdevv)
}
res <- findCovMat(R)
meanv    <- res[[1]]
cov_mat  <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]

checkCovMat <- function(cov_mat) {
  #Check for duplicate covariances:
  D = dim(cov_mat)[1]
  for(d in 1:D)
    for(e in d:D) {
      print(paste(d,e,cov_mat[d,1],cov_mat[e,1]))
      if(d != e && !is.na(cov_mat[d,1]) &&
           !is.na(cov_mat[e,1]) && cov_mat[d,1] == cov_mat[e,1])
        stop(paste("dups in cov_mat",d,e))
    }
}
checkCovMat(cov_mat)

library(tseries)
getHistPrices <- function(lab,w,len,start="2013-11-29",
                          end="2014-11-28",startBck1="2013-11-28",
                          startFwd1="2013-11-27",cached=NA) {
  #gather recent prices for all lab symbols
  D <- length(lab)
  recentPrices = matrix(rep(NA,len*D),nrow=len,ncol=D)
  for(d in 1:D) {
    if(w[d] > 0.0) {
      print(lab[d]) #Use cached list for now-obsolete tickers
      if(!is.na(cached) && !is.na(match(lab[d],cached))) {
         x <- read.csv(paste("cached",lab[d],".csv",sep=""))[,1]
         recentPrices[,d] <- x
      } else
        tryCatch({
          x <- get.hist.quote(lab[d],quote="Adj",start=start,end=end)
          if(length(x) != len) {
            x <- get.hist.quote(lab[d],quote="Adj",
                                start=startBck1,end=end)
            if(length(x) != len) {
              x <- get.hist.quote(lab[d],
                                  quote="Adj",start=startFwd1,end=end)
            } else { #partial quotes
              recentPrices[1:length(x),d] <- x
            }
          } else {
            recentPrices[,d] <- x
          }
        }, warning = function(w) {
          #warning-handler-code
          #print(w)
        }, error = function(e) {
          #error-handler-code
          #print(e)
        })
    }
  }
  return(recentPrices)
}
#unit test: 
library(tseries)
get.hist.quote("BRK-B",start="2016-05-01",end="2017-05-01",quote="Adj")
pdf <- getHistPrices(c('BKNG','MCD'),c(.5,.5),251,
              start="2013-02-15",end="2014-02-14")
pdf
