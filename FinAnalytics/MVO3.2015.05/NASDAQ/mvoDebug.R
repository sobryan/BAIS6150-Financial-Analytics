system("tail /home/mark/FinAnalytics/MVO3.2015.05/NASDAQ/cachedSBUX.csv")
get.hist.quote("SBUX",start="2015-05-15",end="2015-05-20",quote="AdjClose")
print("fine.")

obsDays <- 82
res <- findAllPrices("MVO3.2015.05")
prices0 <- res[[1]]; lab0 <- res[[2]]
par(mfrow=c(1,3))
plot(prices0[,match("SBUX",lab0)],type="l",ylim=c(0,60))
prices0 <- prices0[(nrow(prices0)-(obsDays-1)):nrow(prices0),]
plot(prices0[,match("SBUX",lab0)],type="l",ylim=c(0,60))
print("fine.")

par(mfrow=c(1,3))
plot(prices2[,match("SBUX",lab2)],type="l",
     ylim=c(0,60),xlab="2013-05-20 82 days back")
plot(prices1[,match("SBUX",lab1)],type="l",
     ylim=c(0,60),xlab="2014-05-20 82 days back")
plot(prices0[,match("SBUX",lab0)],type="l",
     ylim=c(0,60),xlab="2015-05-20 82 days back")

par(mfrow=c(1,3))
plot(prices2[,match("WMT",lab2)],type="l",
     ylim=c(0,90),xlab="2013-05-20 82 days back")
plot(prices1[,match("WMT",lab1)],type="l",
     ylim=c(0,90),xlab="2014-05-20 82 days back")
plot(prices0[,match("WMT",lab0)],type="l",
     ylim=c(0,90),xlab="2015-05-20 82 days back")

#We no longer see problems with these prices!
#for acquireCoalescedPrices
