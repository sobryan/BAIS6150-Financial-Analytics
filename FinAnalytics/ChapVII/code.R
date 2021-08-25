#Chapter VII

pruneBySharpe <- function(prices,lab,meanv,sdevv,threshSR,mufree=0) {
  par(mar=c(4,4,1,1))
  par(mfrow=c(1,2))
  indepSharpes <- (meanv-mufree)/sdevv
  len = length(indepSharpes)
  plot(indepSharpes,ylab="SR",col=4)
  plot(sort(indepSharpes),ylab="SR",col=4)
  lines(1:len,rep(threshSR,len))
  indHighSharpes <- (indepSharpes > threshSR)
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
  list(newPrices,newLab,indepSharpes[indHighSharpes])
}
#unit test:
library(huge)
data(stockdata)
D <- length(stockdata$data[1,])
p <- stockdata$data[,1:D]
l <- stockdata$info[1:D,1]
###added 2017-09-16:
for(i in 1:D)  #Need to splitAdjust():
  p[,i] <- splitAdjust(p[,i],l[i])
###
r <- findR(p)
res <- findCovMat(r)
meanv    <- res[[1]]
cov_mat  <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]

###was until 2017-09-16: res <- pruneBySharpe(p,l,meanv,sdevv,.035)
res <- pruneBySharpe(p,l,meanv,sdevv,.057)
p   <- res[[1]]
l   <- res[[2]]
r   <- findR(p)
D   <- length(l)
indepSharpes <- res[[3]]
print(paste('D =',D))

isnaCheckCovMat <- function(R) {
  cov_mat = cov(R);  #plot3d(cor_mat)
  print("Checking cov data.")
  isNACov <- FALSE
  for(d in 1:D) { #check one row for bad data
    if(is.na(cov_mat[d,1])) {
      print(paste("NA for",d,lab[d]))
      cat(lab[d],file="badsyms.txt",append=TRUE,sep="\n")
      isNACov <- TRUE
    }
  }
  if(isNACov) stop("NA Covs recorded in badsyms.txt")
  diag_cov_mat <- diag(cov_mat)
  sdevv        <- sqrt(diag_cov_mat)
}
sdevv <- isnaCheckCovMat(r)

checkDeterminant <- function(prices,R,lab,isSubDir=TRUE) {
  #incrementally build cov_mat to find singularities
  subdirStr = ifelse(isSubDir,"/NYSE","")
  D <- dim(R)[2]
  #First find out which pairs might be too cor
  scalar_cov = vector(length=D)
  for(d in 1:D){
    scalar_cov[d] = cor(R[,d],R[,8])
    print(paste(d,round(scalar_cov[d],6)))
  }
  #Look specifically for consecutive same prices[20,d],prices[20,d+1]
  for(d in 1:(D-1))
    if(prices[20,d] == prices[20,d+1]) { #arb pick 20th time point to compare
      print("adding to badcors.txt")
      print(lab[d:(d+1)])
      system(paste("echo",lab[d],
        paste(">> ",homeuser,"/FinAnalytics/",dir,subdirStr,
              "/badcors.txt",sep="")))
    }
  for(d in 5:D){
    Rsmall = R[,1:d]
    small_cov_mat = cor(Rsmall)
    deter = det(small_cov_mat)
    print(paste(d,lab[d],deter,dim(Rsmall)[2]))
    if(deter <= 0.0) {
      system(paste("echo",lab[d],
        paste(">> ",homeuser,"/FinAnalytics/",dir,subdirStr,
              "/badcors.txt",sep="")))
      stop(paste(d,lab[d],"det =",deter))
    }
  }
}
checkDeterminant(p,r,l)

if(F){ #Unsupported
library(quantmod)
symbol='GOOG'
getFinancials(symbol, src="google")
GOOG.f$IS$A["Diluted Normalized EPS",]
20.72/19.77
}

library(tseries)
library(quantmod)
dir <- 'MVO4'
len <- 1006
isQtrly = FALSE
if(isQtrly) back = 5 else back = 4
if(isQtrly) stmt = 'Q' else stmt = 'A'
res <- readSubDirs(dir)
D1  <- res[[1]]
D2  <- res[[2]]
lab <- res[[3]]
D <- D1 + D2

start <- "2011-02-09"
end   <- "2015-02-09"
isPlotInAdjCloses <<- FALSE
isCacheEnabled    <<- TRUE
prices <- matrix(rep(NA,len*D),nrow=len,ncol=D)
#Must run acquirePrices if cache files do not yet exist:
library(tseries)
prices <- acquirePrices(prices,lab,len,D,D1,D2,dir,
                        start,end,isSubDir=TRUE)
dir <- 'QMDM'
createDirs(dir,isSubDir=FALSE)

readAndCleanISDF <- function(expectedLab,
                             dir='QMDM',stmt='A') {
  setwd(paste(homeuser,"/FinAnalytics/",dir,"/",sep=""))
  fn <- paste("IncomeStmts",stmt,".csv",sep="")
  #File must exist
  if(file.exists(fn)) {
    ISDF <- read.csv(fn,header = TRUE)
    relevantLab <- intersect(expectedLab,ISDF[,1])
    expectedD <- length(expectedLab)
    #count number of matching tickers: must have at least half
    if(length(relevantLab) > .50*expectedD) {
        #Remove entries with missing income stmt info
        cleanedISDF <- na.omit(ISDF)
        lab <- as.character(cleanedISDF[,1])
        D <- length(lab)
        cleanedISDF
    } else NA #missing income stmt recs
  } else NA #no file
}

obtainIncomeStmtFigures <- function(lab,dir='QMDM',isQtrly=TRUE) {
  #Read income stmt records via quantmod package
  #Only need to execute once for ETF
  D = length(lab)
  if(isQtrly) back = 5 else back = 4
  if(isQtrly) stmt = 'Q' else stmt = 'A'
  ncol = (2+4*back)
  #Try to read cached income stmts
  ISDF <- readAndCleanISDF(lab,
              dir=dir,stmt='A')
  if(!is.null(dim(ISDF))) return(ISDF)
  print("Income stmt file not found: using getFinancials()")
  ISDF <- data.frame(matrix(nrow=D,ncol=ncol))
  #colnams(ISDF) <- c("symbol","netinc",
  #  "totrev3yr","gsprof3yr","dneps3yr")
  for(d in 1:D) {
    symbol = lab[d]
    basedate = NA
    netinc = rep(NA,back); totrev = rep(NA,back)
    gsprof = rep(NA,back); dneps  = rep(NA,back)
    print(symbol)
    isFound <- TRUE
    tryCatch( {
      getFinancials(symbol, src="google")
    }, error = function(e) {
      print(e); isFound <- FALSE
      netinc <- rep(NA,back); totrev <- rep(NA,back)
      gsprof <- rep(NA,back); dneps  <- rep(NA,back)
    } )
    if(isFound) {
      tryCatch( {
        Net.Income<-eval(parse(text=paste(
          symbol,'.f$IS$',stmt,'["Net Income",]',sep='')))
        if(is.numeric(Net.Income[1])) {
          netinc = round(Net.Income,2)
        } else {
          netinc = rep(NA,back)
        }
      }, error = function(e) {
        print(e); netinc <- rep(NA,back)
      } )
      tryCatch( {
        Total.Revenue<-eval(parse(text=paste(
          symbol,'.f$IS$',stmt,'["Revenue",]',sep='')))
        if(is.numeric(Total.Revenue[1])) {
          totrev = round(Total.Revenue,2)
        } else {
          totrev = rep(NA,back)
        }
      }, error = function(e) {
        print(e); totrev <- rep(NA,back)
      } )
      tryCatch( {
        Gross.Profit<-eval(parse(text=paste(
          symbol,'.f$IS$',stmt,'["Gross Profit",]',sep='')))
        if(is.numeric(Gross.Profit[1])) {
          gsprof = round(Gross.Profit,2)
        } else {
          gsprof = rep(NA,back)
        }
      }, error = function(e) {
        print(e); gsprof <- rep(NA,back)
      } )
      tryCatch( {
        Dil.Norm.EPS<-eval(parse(text=paste(
          symbol,'.f$IS$',stmt,'["Diluted Normalized EPS",]',sep='')))
        if(is.numeric(Dil.Norm.EPS[1])) {
          basedate = names(Dil.Norm.EPS)[1]
          dneps = round(Dil.Norm.EPS,2)
        } else {
          dneps = rep(NA,back)
        }
      }, error = function(e) {
        print(e); dneps <- rep(NA,back)
      } )
    }
    #print(basedate)
    items = c(symbol,basedate,netinc,totrev,gsprof,dneps)
    if(length(items) == ncol)
      ISDF[d,] = items
  }
  #ISDF #return income stmt net 3yr growth rates
  ISDF
}

writeISDF <- function(ISDF,dir='QMDM',stmt='A') {
  createDirs(dir)
  labNYSE <- as.character(
    read.csv("NYSE/NYSEclean.txt",
             header=TRUE,sep="\t")[,1])
  labNASQ <- as.character(
    read.csv("NASDAQ/NASDAQclean.txt",
             header=TRUE,sep="\t")[,1])
  lab <- c(labNYSE,labNASQ)
  
  ISDF <- obtainIncomeStmtFigures(lab,dir,isQtrly)
  savedISDF <- ISDF
  colnames(ISDF) <- c("symbol","basedate",
                      paste("netinc",0:(back-1),sep=""),
                      paste("totrev",0:(back-1),sep=""),
                      paste("gsprof",0:(back-1),sep=""),
                      paste("dneps",0:(back-1),sep=""))
  fileName = paste("IncomeStmts",stmt,".csv",sep="")
  write.csv(ISDF,fileName,row.names = FALSE)
}
#Check first to see if run is necessary
if(!file.exists(paste(homeuser,"/FinAnalytics/",dir,"/IncomeStmts",
                      stmt,".csv",sep=""))) {
  writeISDF(ISDF,stmt=stmt)
}
ISDF <- obtainIncomeStmtFigures(lab,dir='QMDM',isQtrly=FALSE)
dim(ISDF)
if(F) {#3 lines deprecated no longer operational:
symbol='PBIB'
getFinancials(symbol, src="google")
PBIB.f$IS$A["Net Income",]
}#
#Compute gross returns or growth rates
#Use abs() and sign() to force NA when not positive
calcGth <- function(a,b) {
  if(is.na(a) || is.infinite(a) ||
       is.na(b) || is.infinite(b) || abs(a) < .001)
    return(NA)
  if(sign(a) == -1 && sign(b) == -1)
    return((-abs(b)/abs(a)))
  if(sign(a) == -1 && sign(b) == +1)
    return(NA)#((-a+b)/-a)
  if(sign(a) == +1 && sign(b) == -1)
    return(NA)#(-(a+abs(b))/a)
  return(round(abs(b)/abs(a),2)*sign(b))
}
#Unit tests:
calcGth(1.25,1.75)
calcGth(-1.25,1.75)
calcGth(1.25,-1.75)
calcGth(-1.25,-1.75)
calcGth(-1.25,NA)
calcGth(1/0,1.75)
calcGth(.0005,1.75)

plotIncomeStmtGth <- function(ISDF,back) {
#input: income stmt data frame: D x 17
  par(mar=c(4,4,2,1))
  par(mfrow=c(2,2))
  mapToCol <- function(d)
    if(d==7) 1 else if(d==8)
      2 else if(d==15) 3 else if(d==23) 4 else d
  mainVec = c("Net Income Growth","Total Revenue Growth",
              "Gross Profit Growth","Diluted Norm EPS Growth")
  D = dim(ISDF)[1]
  for(initFld in 2+c(1:4*back)) {
    isPlotted = FALSE
    for(d in 1:D) {
      symbol = as.character(ISDF[d,1])
      print(symbol)
      finalFld = initFld - (back-1)
      initAmt = as.double(ISDF[d,initFld])
      finalAmts = as.double(ISDF[d,initFld:finalFld])
      gthAmts = c()
      for(i in 1:back) 
        gthAmts = c(gthAmts,calcGth(initAmt,finalAmts[i]))
      print(gthAmts)
      if(initFld == 2+4*back) ylim=c(0.5,3.0) else ylim=c(0.5,3.0)
      if(d == 1 || !isPlotted) { #initFld is gth baseline col
        if(!is.na(gthAmts[1])) {
           isPlotted = TRUE
           plot(gthAmts,xlab="Years",
             type='o',ylim=ylim,ylab="Gross Return",
             main=mainVec[(initFld-1)/back])
        }
      } else {
        if(!is.na(gthAmts[1]))
          lines(gthAmts,type='o',
             col=mapToCol(d))
      }
      if(!is.na(gthAmts[1]))
        text(back-.05,gthAmts[back]-.01,symbol,cex=.75)
    }
    cols <- sapply(c(1:D),mapToCol)
    print("------------")
  }
}
#Unit test:
ISDFSlice=ISDF[(match('PCLN',ISDF[,1])-3):
                 (match('PCLN',ISDF[,1])+6),]
ISDFSlice
plotIncomeStmtGth(ISDFSlice,back)

ISDFSlice=ISDF[(match('UNP',ISDF[,1])-0):
                   (match('UNP',ISDF[,1])+2),]
ISDFSlice

#Take time out and look at the basedates.
par(mfrow=c(1,2))
dvec <- as.Date(ISDF[,2])
plot(dvec,ylab="basedate",col=4)
hist(dvec,breaks=100,col=4,ylab="basedate",main="")
maxd = max(dvec)
maxd
#Below we can see the population of end periods:
sum(dvec=="2014-12-31")/length(dvec)
sum(dvec=="2014-09-30")/length(dvec)
sum(dvec=="2014-06-30")/length(dvec)
sum(dvec=="2014-03-31")/length(dvec)
sum(dvec=="2013-12-31")/length(dvec)

####################################

findGth <- function(ISDF) {
  ISgthDF <- ISDF[,c(1:14)] #sets schema
  ISgthDF[,3] <- mapply(calcGth,ISDF[,6],ISDF[,5])
  ISgthDF[,4] <- mapply(calcGth,ISDF[,10],ISDF[,9])
  ISgthDF[,5] <- mapply(calcGth,ISDF[,14],ISDF[,13])
  ISgthDF[,6] <- mapply(calcGth,ISDF[,18],ISDF[,17])
  
  ISgthDF[,7] <- mapply(calcGth,ISDF[,5],ISDF[,4])
  ISgthDF[,8] <- mapply(calcGth,ISDF[,9],ISDF[,8])
  ISgthDF[,9] <- mapply(calcGth,ISDF[,13],ISDF[,12])
  ISgthDF[,10] <- mapply(calcGth,ISDF[,17],ISDF[,16])
  
  ISgthDF[,11] <- mapply(calcGth,ISDF[,4],ISDF[,3])
  ISgthDF[,12] <- mapply(calcGth,ISDF[,8],ISDF[,7])
  ISgthDF[,13] <- mapply(calcGth,ISDF[,12],ISDF[,11])
  ISgthDF[,14] <- mapply(calcGth,ISDF[,16],ISDF[,15])
  
  ISgthDF[,1] <- as.character(ISDF[,1])
  #dnepsgth2 means Dil.Net.EPS gth based upon two
  #figures: one 2 years back and one 3 years back  
  colnames(ISgthDF) <- c("symbol","basedate",
      "netincgth2","totrevgth2","gsprofgth2","dnepsgth2",
      "netincgth1","totrevgth1","gsprofgth1","dnepsgth1",
      "netincgth0","totrevgth0","gsprofgth0","dnepsgth0")
  rownames(ISgthDF) <- NULL
  ISgthDF
}

####################################
ISgthDF <- findGth(ISDF)
cleanedISgthDF <- na.omit(ISgthDF)
ISgthDF <- cleanedISgthDF
ISgthDFSlice=ISgthDF[(match('UNP',ISgthDF[,1])-1):
                 (match('UNP',ISgthDF[,1])+1),]
ISgthDFSlice

ISgthDF[c(match('CNI',ISgthDF[,1]),
          match('KSU',ISgthDF[,1]),
          match('NSC',ISgthDF[,1]),
          match('UNP',ISgthDF[,1])),]
lab <- as.character(ISgthDF[,1])
D   <- length(lab)

findIncomeStmtSR <- function(ISgthDF,cols,
                    main="") {
  #Find Income Stmt Sharpe Ratio
  SRvec <- apply(ISgthDF[,cols],1,mean)/
    apply(ISgthDF[,cols],1,sd)
  plot(SRvec,cex=0,main=main,col=4)
  text(SRvec,ISgthDF[,1],col=4,cex=.5)
  SRvec
}

findBestAllIncomeStmtSR <- function(
  vecSR1, vecSR2, vecSR3, vecSR4, thresh=50) {
  #From 4 SR vectors, find those that meet thresh
  indVec1SR = vecSR1 > thresh
  indVec2SR = vecSR2 > thresh
  indVec3SR = vecSR3 > thresh
  indVec4SR = vecSR4 > thresh
  indAllSR = indVec1SR & indVec2SR & indVec3SR & indVec4SR
  indAllSR
}

par(mfrow=c(2,2))
cols  <- c(3,7,11)
ignSR <- findIncomeStmtSR(ISgthDF,cols,
         main="Net Income Gth SR")

cols  <- c(4,8,12) #totrevgth2, totrevgth1, totrevgth0
trgSR <- findIncomeStmtSR(ISgthDF,cols,
         main="Total Revenue Gth SR")

cols  <- c(5,9,13) #gsprofgth2, gsprofgth1, gsprofgth0
gpgSR <- findIncomeStmtSR(ISgthDF,cols,
         main="Gross Profit Gth SR")

cols  <- c(6,10,14) #dnepsgth2, dnepsgth1, dnepsgth0
esgSR <- findIncomeStmtSR(ISgthDF,cols,
         main="Earning per Share Gth SR")

#Let us look at price charts for top two of each
#PLL no longer exists as a ticker, May, 2015
topSRlab <- c('ROL','JKHY','WIT','ULTI', ##PLL becomes DHR
              'POL','BWLD','DHR','WAB') ##modified August 28, 2016
prices <- getHistPrices(topSRlab,rep(1/8,8),252*5-2, #WAS: -1 #mod 12/11/2018
          start="2010-07-01",end="2015-06-30",
          startBck1="2010-06-30",startFwd1="2010-07-02")
plotMultSeries(prices,topSRlab,rep(1/8,8),8,
               cc="days",ret="",ylim=c(.6,5.5))

ISgthSRDF <- data.frame(as.character(ISgthDF[,1]),
                        ignSR,trgSR,gpgSR,esgSR)
colnames(ISgthSRDF) <- c("symbol","ignSR",
                       "trgSR","gpgSR","esgSR")
cleanedISgthSRDF <- na.omit(ISgthSRDF)
ISgthSRDF <- cleanedISgthSRDF
ISgthSRDF[match('UNP',ISgthSRDF[,1]),] #sample
ISgthSRDF[match('INTC',ISgthSRDF[,1]),] #sample
ISgthDF[match('UNP',ISgthDF[,1]),] #sample
ISgthDF[match('INTC',ISgthDF[,1]),] #sample

ind8SR <- findBestAllIncomeStmtSR(
  ignSR,trgSR,gpgSR,esgSR,thresh=40)
sum(ind8SR)
top8SRlab <- as.character(ISgthSRDF[,1])[ind8SR]
top8SRlab

prices <- getHistPrices(top8SRlab,rep(1/8,8),252*5-2, #WAS: -1 #mod 12/11/2018
                        start="2010-07-01",end="2015-06-30",
                        startBck1="2010-06-30",startFwd1="2010-07-02")
plotMultSeries(prices,top8SRlab,rep(1/8,8),8,
               cc="days",ret="",ylim=c(.6,5.5))

indAllSR <- findBestAllIncomeStmtSR(
            ignSR,trgSR,gpgSR,esgSR,thresh=25)
sum(indAllSR)
topSRlab <- as.character(ISgthSRDF[,1])[indAllSR]
D = length(topSRlab)
len = dim(ISgthSRDF)[1]
topSRlab[9]<-"KMI"; topSRlab[15]<-"BRK-B"; topSRlab ##
prices <- getHistPrices(topSRlab,rep(1/D,D),252*5-1,
                        start="2010-03-16",end="2015-03-15",
                        startBck1="2010-03-15",startFwd1="2010-03-17")
plotMultSeries(prices,topSRlab,rep(1/D,D),D,
               cc="days",ret="",ylim=c(.6,12))

plotIncomeStmtSRTops <- function(isSRvec,indAllSR,
                        lab,minSR,maxSR,type=1) {
  set.seed(200)
  par(mar=c(4,4,2,1))
  par(mfrow=c(1,1))
  numPoints = length(isSRvec[indAllSR])
  if(type == 1) {
    plot(rep(type,numPoints),isSRvec[indAllSR],cex=0,
      xlim=c(0,5),main="All Income Stmt Gth SR",
      ylim=c(minSR,maxSR),xlab="Income Stmt Gth Type",ylab="SR")
  } else {
    points(rep(type,numPoints),isSRvec[indAllSR],cex=0)
  }
  text(rep(type,numPoints)+.20*rnorm(numPoints),
       isSRvec[indAllSR],ylim=c(minSR,maxSR),
       as.character(lab[indAllSR]),cex=.75,col=type)
}
maxSR <- max(ignSR[indAllSR],trgSR[indAllSR],
             gpgSR[indAllSR],esgSR[indAllSR])
minSR <- min(ignSR[indAllSR],trgSR[indAllSR],
             gpgSR[indAllSR],esgSR[indAllSR])
plotIncomeStmtSRTops(ignSR,indAllSR,ISgthSRDF[,1],minSR,maxSR,1)
plotIncomeStmtSRTops(trgSR,indAllSR,ISgthSRDF[,1],minSR,maxSR,2)
plotIncomeStmtSRTops(gpgSR,indAllSR,ISgthSRDF[,1],minSR,maxSR,3)
plotIncomeStmtSRTops(esgSR,indAllSR,ISgthSRDF[,1],minSR,maxSR,4)

#track UA's IS SR for all 4 categories
UAidx = match('UA',ISgthSRDF[,1])
ignSR[UAidx]; trgSR[UAidx]; gpgSR[UAidx]; esgSR[UAidx]
