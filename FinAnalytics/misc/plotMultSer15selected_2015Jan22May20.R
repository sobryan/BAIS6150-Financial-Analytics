library(tseries)

lab <- c("AMRC","ATU","HRS","ICB","INXN","MIC","RRC","UFS","WM",
         "GT","LAMR","LMNX","LRCX","SATS","SBUX","SHPG","UEPS","VOD")
D <- length(lab)
w=rep(1/D,D)

start0 = "2015-01-22"
end0   = "2015-05-20"
#obtain S&P prices firstly and find obsDays:
pricesSP0 <- get.hist.quote("^GSPC",quote="AdjClose",start=start0,end=end0)
tradeDays <- length(pricesSP0)

#Now find prices for portfolio of stocks:
pricesD <- getHistPrices(lab,w,tradeDays,
                         start=start0,end=end0)
lab
pricesD[1,]
lab <- lab[!is.na(pricesD[1,])]
pricesD <- pricesD[,!is.na(pricesD[1,])]
dim(pricesD)
D <- length(lab)
w=rep(1/D,D)
plotMultSeries(pricesD,lab,w,D,cc=NA,ret=NA,
               ylim=c(.75,1.35))
lines(as.numeric(pricesSP0)/as.numeric(pricesSP0[1]),col=4,lw=4)
spGrossReturn <- as.numeric(pricesSP0[tradeDays])/as.numeric(pricesSP0[1])

pv <- weightPortOOS(lab,tradeDays,D,w,prices=pricesD,
                          start=start0,end=end0,
                          #startBck1="2013-11-28",startFwd1="2013-11-27",
                          isNaive=TRUE) 
pv
lines(pv/pv[1],col=3,lw=4)
