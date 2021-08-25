#Chapter XI
setwd(paste(homeuser,"/FinAnalytics/ChapXI",sep=""))

reset <- function(S) {
  print("reset")
  K <<- 5; KH <<- 10
  LONG <<- 1; SHRT <<- 2; PROF <<- 1; LOSS <<- 2
  MaxTrades <<- round(length(S)/100)
  longProfTicks <<- 0
  longLossTicks <<- 0
  shrtProfTicks <<- 0
  shrtLossTicks <<- 0
  longProfLogDiffS <<- array(rep(0,MaxTrades,KH),c(MaxTrades,KH));
  longProfIdx <<- 1
  longLossLogDiffS <<- array(rep(0,MaxTrades,KH),c(MaxTrades,KH));
  longLossIdx <<- 1
  shrtProfLogDiffS <<- array(rep(0,MaxTrades,KH),c(MaxTrades,KH));
  shrtProfIdx <<- 1
  shrtLossLogDiffS <<- array(rep(0,MaxTrades,KH),c(MaxTrades,KH));
  shrtLossIdx <<- 1
  longProf <<- vector(length=MaxTrades)
  longLoss <<- vector(length=MaxTrades)
  shrtProf <<- vector(length=MaxTrades)
  shrtLoss <<- vector(length=MaxTrades)
  logProfIdx <<- 1
  longLossIdx <<- 1
  shrtProfIdx <<- 1
  shrtLossIdx <<- 1
}

reportCounts <- function() {
  print(paste(longProfIdx-1,longLossIdx-1,
              shrtProfIdx-1,shrtLossIdx-1))
  print(paste("this sim longs =",counts[1],"shrts = ",counts[2]))
  winTicks = longProfTicks+shrtProfTicks
  totalTicks = longProfTicks+shrtProfTicks-
    longLossTicks-shrtLossTicks
  print(paste(round(winTicks),round(totalTicks),
              "winning ratio:", round(winTicks / totalTicks,4)))
  annHistVol
}

ec = read.csv("ECprices201310.csv")[,1]
diff(log(ec[40:100])) > 0

draws = rbeta(1000000,7,2)
mean(draws)

isLongIndicator <- function(logRetArr) {
  Y = sum(logRetArr[1:5] > 0)
  return(Y == 5)
}
isShrtIndicator <- function(logRetArr) {
  Y = sum(logRetArr[1:5] < 0)
  return(Y == 5)
}

displayLogRetInds <- function(r,times) {
  plot(times,r,type='l',col=4,
       ylim=c(-.0015,.0015))
  points(times,r,cex=.2)
  len = length(r)
  lines(times,rep(0,len)) #plot x-axis
  longInd <- as.vector(rep(0,10)); j <<- 0
  shrtInd <- as.vector(rep(0,10)); k <<- 0
  for(i in 5:length(r)) {
    t = times[1]+i-1
    if(isLongIndicator(r[(i-4):i])) {
      j <<- j + 1
      longInd[j] <- i
      i = i + 5
      print(t)
    }
    if(isShrtIndicator(r[(i-4):i])) {
      k <<- k + 1
      shrtInd[k] <- i
      i = i + 5
      print(t)
    }
  }
  #Draw the potential entries:
  if(j>0)
    for(i in 1:j)
      lines(c(longInd[i]+times[1]-1,
              longInd[i]+times[1]-1),
            c(-.0015,.0015),col="green")
  if(k>0)
    for(i in 1:k)
      lines(c(shrtInd[i]+times[1]-1,
              shrtInd[i]+times[1]-1),
            c(-.0015,.0015),col="yellow")
  longInd <<- longInd
  shrtInd <<- shrtInd
}

S = ec
logDiffS = diff(log(S))
logDiffSmean = mean(logDiffS)
N = length(logDiffS)
minHistVol = sqrt(1/(N-1)*sum((logDiffS-logDiffSmean)^2))
annHistVol = minHistVol*sqrt(60*24*252)
annHistVol

sim <- function(S,mo,stopAmt=.0035,profAmt=.0045) {
  plot(S,type='l',col='blue4',xlab='minutes',
       ylab='EUR in USD')
  if(TRUE) #"blue"
    points(S,type='p',xlab="Minutes",ylab="EC",
           col='blue4',pch=16, cex=1.4)
  #simulate strategies on the incoming chart
  #We use 1e4 multiplier since logrets are only used for
  #pattern analysis
  logDiffS = 10000*diff(log(S))
  logDiffS = append(logDiffS, 0.0, after = 0); #log ret inds
  logDiffSmean = mean(logDiffS)
  N = length(logDiffS)
  minHistVol <<- sqrt(1/(N-1)*
                sum((logDiffS/1e4-logDiffSmean/1e4)^2))
  annHistVol <<- minHistVol*sqrt(60*24*252)
  tradetrange = 0
  tradeSrange = 0
  direction = 0
  countT = 0; countF = 0
  logDiffSentry = array(rep(0,KH),c(KH))
  
  i = 11
  while(i<=length(logDiffS)) {
    #long:
    if((direction == 0) && isLongIndicator(logDiffS[(i-4):i])
    ) {
      logDiffSentry = logDiffS[(i-(KH-1)):i]
      direction = +0.25 #buy upon next points
    }
    else if(direction == +0.25) {
      tradetrange = c(i)
      tradeSrange = c(S[i])
      print(paste("long: ",tradetrange,tradeSrange))
      countT <- countT + 1
      i <- i + 5 #fast fwd time for next indicator instance
      direction = +1
    }
    #long unwind:
    if((direction == +1) && ((S[i]-tradeSrange[1]) > profAmt)) {
      direction = +0.75
    }
    else if(direction == +0.75) {
      tradetrange = union(tradetrange, c(i))
      tradeSrange = union(tradeSrange, c(S[i]))
      print(paste("unwind long expected gain: ",tradetrange[2],
                  tradeSrange[2],round(tradeSrange[2]-tradeSrange[1],5)))
      lines(tradetrange,tradeSrange,type="l",col="green",lwd=3)
      points(tradetrange[2],tradeSrange[2],
             cex=2,pch="*",col="green")
      longProf[longProfIdx] <<-
        tradeSrange[2]-tradeSrange[1] >= 0
      longProfLogDiffS[longProfIdx,] <<- logDiffSentry
      longProfIdx <<- longProfIdx + 1
      longProfTicks <<- longProfTicks + 10000*
        (tradeSrange[2] - tradeSrange[1])
      tradetrange = 0; tradeSrange = 0; direction = 0
    }
    #long unwind:
    if((direction == +1) && ((S[i]-tradeSrange[1]) <= -stopAmt)) {
      direction = +0.50
    }
    else if(direction == +0.5) {
      tradetrange = union(tradetrange, c(i))
      tradeSrange = union(tradeSrange, c(S[i]))
      print(paste("unwind long expected loss: ",tradetrange[2],
                  tradeSrange[2],round(tradeSrange[2]-tradeSrange[1],5)))
      lines(tradetrange ,tradeSrange ,type="l",col="brown",lwd=3)
      points(tradetrange[2],tradeSrange[2],
             cex=3,pch="-",col="brown")
      longLoss[longLossIdx] <<-tradeSrange[2]-tradeSrange[1] < 0
      longLossLogDiffS[longLossIdx,] <<- logDiffSentry
      longLossIdx <<- longLossIdx + 1
      longLossTicks <<- longLossTicks + 10000*
        (tradeSrange[2] - tradeSrange[1])
      tradetrange = 0; tradeSrange = 0; direction = 0
    }
    #short:
    if((direction == 0) && isShrtIndicator(logDiffS[(i-4):i])
    ) {
      logDiffSentry = logDiffS[(i-(KH-1)):i]
      direction = -0.25
    }
    else if(direction == -0.25) {
      tradetrange = c(i)
      tradeSrange = c(S[i])
      print(paste("shrt: ",tradetrange,tradeSrange))
      countF <- countF + 1
      i <- i + 5 #fast fwd time for next indicator instance
      direction = -1
    }
    #short unwind:
    if((direction == -1) && ((tradeSrange[1]-S[i]) > profAmt)) {
      direction = -0.75
    }
    else if(direction == -0.75) {
      tradetrange = union(tradetrange, c(i))
      tradeSrange = union(tradeSrange, c(S[i]))
      print(paste("unwind shrt expected gain: ",tradetrange[2],
                  tradeSrange[2],round(tradeSrange[1]-tradeSrange[2],5)))
      lines(tradetrange ,tradeSrange ,type="l",
            col="gold",lwd=3)
      points(tradetrange[2],tradeSrange[2],
             cex=2,pch="*",col="gold")
      shrtProf[shrtProfIdx] <<- tradeSrange[1]-tradeSrange[2] >= 0
      shrtProfLogDiffS[shrtProfIdx,] <<- logDiffSentry
      shrtProfIdx <<- shrtProfIdx + 1
      shrtProfTicks <<- shrtProfTicks + 10000*
        (tradeSrange[1] - tradeSrange[2])
      tradetrange = 0; tradeSrange = 0; direction = 0
    }
    if((direction == -1) && ((tradeSrange[1]-S[i]) <= -stopAmt)) {
      direction = -0.50
    }
    else if(direction == -0.50) {
      tradetrange = union(tradetrange, c(i))
      tradeSrange = union(tradeSrange, c(S[i]))
      print(paste("unwind shrt expected loss: ",tradetrange[2],
                  tradeSrange[2],round(tradeSrange[1]-tradeSrange[2],5)))
      lines(tradetrange,tradeSrange,type="l",col="red",lwd=3)
      points(tradetrange[2],tradeSrange[2],
             cex=3,pch="-",col="red")
      shrtLoss[shrtLossIdx] <<-tradeSrange[1]-tradeSrange[2] < 0
      shrtLossLogDiffS[shrtLossIdx,] <<- logDiffSentry
      shrtLossIdx <<- shrtLossIdx + 1
      shrtLossTicks <<- shrtLossTicks+10000 *
        (tradeSrange[1] - tradeSrange[2])
      tradetrange = 0; tradeSrange = 0; direction = 0
    }
    i <- i + 1
  }
  return(c(countT,countF))
}

setwd(paste(homeuser,"/FinAnalytics/ChapXI",sep=""))
par(mfrow=c(2,1))
start=890; end=920 #Limits chart to start:end
ec = read.csv("ECprices201306.csv")[,1]
ec = ec[start:end]
diffLogEC = diff(log(ec))
times=c(start:(end-1))
countInd(diffLogEC)
displayLogRetInds(diffLogEC,times)
ec = read.csv("ECprices201306.csv")[,1]
plot(start:end,ec[start:end],type="p",col=4)
reset(ec[1:800])
counts <- sim(ec[1:800],"01306",
              stopAmt=.0045,profAmt=.0025)
reportCounts()
ec = read.csv("ECprices201306.csv")[,1]
plot(ec[1:5000],type="l",col=4)
reset(ec[1:5000])
counts <- sim(ec[1:5000],"201306",
              stopAmt=.0045,profAmt=.0025)
reportCounts()

setwd(paste(homeuser,"/FinAnalytics/ChapXI",sep=""))
ec = read.csv("ECprices201305.csv",header = FALSE)[,1]
reset(ec)
counts <- sim(ec,"201305")
reportCounts()

ec = read.csv("ECprices201306.csv",header = FALSE)[,1]
reset(ec)
counts <- sim(ec,"201306")
reportCounts()

ec = read.csv("ECprices201307.csv",header = FALSE)[,1]
reset(ec)
counts <- sim(ec,"201307")
reportCounts()

ec = read.csv("ECprices201308.csv",header = FALSE)[,1]
reset(ec)
counts <- sim(ec,"201308")
reportCounts()

ec = read.csv("ECprices201309.csv",header = FALSE)[,1]
reset(ec)
counts <- sim(ec,"201309")
reportCounts()

ec = read.csv("ECprices201310.csv",header = FALSE)[,1]
reset(ec)
counts <- sim(ec,"201310")
reportCounts()

#post-simulation analysis of indicator distribution
plotMeanInds <- function() {
  par(mfrow=c(2,2))
  print(longProfLogDiffS[1:(longProfIdx-1),])
  plot(apply(longProfLogDiffS[1:(longProfIdx-1),],2,mean),
       xlab=paste("N =",longProfIdx-1),
       ylab="long prof: 1e5*logrets",ylim=c(-5,5),col=4)
  abline(h = 0,v = 5.5,col=8)
  print(longLossLogDiffS[1:(longLossIdx-1),])
  plot(apply(longLossLogDiffS[1:(longLossIdx-1),],2,mean),
       xlab=paste("N =",longLossIdx-1),
       ylab="long loss: 1e5*logrets",ylim=c(-5,5),col=4)
  abline(h = 0,v = 5.5,col=8)
  print(shrtProfLogDiffS[1:(shrtProfIdx-1),])
  plot(apply(shrtProfLogDiffS[1:(shrtProfIdx-1),],2,mean),
       xlab=paste("N =",shrtProfIdx-1),
       ylab="shrt prof: 1e5*logrets",ylim=c(-5,5),col=4)
  abline(h = 0,v = 5.5,col=8)
  print(shrtLossLogDiffS[1:(shrtLossIdx-1),])
  plot(apply(shrtLossLogDiffS[1:(shrtLossIdx-1),],2,mean),
       xlab=paste("N =",shrtLossIdx-1),
       ylab="shrt loss: 1e5*logrets",ylim=c(-5,5),col=4)
  abline(h = 0,v = 5.5,col=8)
}
plotMeanInds()
