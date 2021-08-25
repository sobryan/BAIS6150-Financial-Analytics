#Chapter XII
library(DBI)
library(RSQLite)
con <- dbConnect(SQLite(),":memory:")
dbWriteTable(con,"mtcars",mtcars)
dbListFields(con,"mtcars")

result <- dbGetQuery(con,"SELECT * FROM mtcars WHERE cyl = 4")
result
result <- dbGetQuery(con,
          "SELECT * FROM mtcars WHERE cyl = 4 AND mpg > 30")
result

dbDisconnect(con)

library(foreign)
setwd(paste(homeuser,"/FinAnalytics/ChapXII",sep=""))
funda <- read.dta("funda.dta")
msf <- read.dta("msf.dta")
con <- dbConnect(SQLite(),":memory:")
dbWriteTable(con,"funda",funda,overwrite=TRUE)
dbWriteTable(con,"msf",msf,overwrite=TRUE)
dbListTables(con)

query <- "SELECT tic, at, lt
FROM funda
WHERE fyear = 2010
AND tic ='IBM'"
result <- dbGetQuery(con,query)
result
query<-"SELECT tic, prcc_c, csho, at-lt AS bv
        FROM funda
        WHERE fyear = 2010
        AND tic ='IBM'"
result <- dbGetQuery(con,query)
result
result$prcc_c * result$csho
#market-to-book (M/B) ratio:
result$prcc_c * result$csho / result$bv

query<-"SELECT tic, at-lt AS bv, prcc_c*csho/(at-lt) AS mb
        FROM funda
        WHERE fyear = 2010
        AND tic ='IBM'"
result <- dbGetQuery(con,query)
result
query <- "SELECT tic, prcc_c*csho/(at-lt) AS mb
        FROM funda
        WHERE fyear = 2010
        AND tic IS NOT NULL
        AND prcc_c IS NOT NULL
        AND csho IS NOT NULL
        AND seq IS NOT NULL"
result <- dbGetQuery(con,query)
result <- subset(result,mb > 0.0 & mb < 50)
summary(result$mb)
hist(result$mb,breaks=100,main="")

query <- "SELECT tsymbol,date,ret
        FROM msf
        WHERE date BETWEEN '2010-01-01' AND '2010-12-31'
        AND tsymbol IN ('AAPL','GOOG')"
result <- dbGetQuery(con,query)
result
c1 <- result[result$tsymbol=='AAPL',]$ret
c2 <- result[result$tsymbol=='GOOG',]$ret
cbind(c1,c2)

library(reshape2)
result <- melt(result,id=c("tsymbol","date"))
result
dcast(result,date~tsymbol+variable)
dcast(result,tsymbol~variable,mean)
mean(c1)
mean(c2)

library(tseries)
prices <- getHistPrices(c('AAPL','GOOGL'),c(.5,.5),252,
                        start="2010-01-01",end="2010-12-31",
                        startBck1="2009-12-31",startFwd1="2010-01-02")
plotMultSeries(prices,c('AAPL','GOOG'),c(.5,.5),2,
               cc="days",ret="",ylim=c(.6,1.5))

query <- "SELECT fyear, sich, (csho*prcc_f)/(at-lt) AS mb
        FROM funda
        WHERE fyear >= 2004
        AND tic IN ('GOOG')"
res1 <- dbGetQuery(con,query)
unique(res1$sich)
query <- "SELECT tic, fyear, (csho*prcc_f)/(at-lt) AS mb
        FROM funda
        WHERE fyear >= 2004
        AND fyear <= 2013
        AND sich = 7370
    AND tic NOT IN ('GOOG')
        AND mb IS NOT NULL
        ORDER BY tic, fyear"
res2 <- dbGetQuery(con,query)
library(reshape2)
res2 <- melt(res2,id=c("tic","fyear"),na.rm=TRUE)
res2 <- dcast(res2, fyear~variable, median)
par(mar=c(4,4,2,2))
plot(res1$fyear,res1$mb,type='l',ylim=c(0,1.1*max(res1$mb)),col='blue',
       xlab='year',ylab='Google M/B ratio versus industry median M/B ratio')
lines(res2$fyear,res2$mb,type='l',col='red')
legend(x=2008,y=15,legend=c("GOOG M/B","industry 7370 M/B"),
       col=c('blue','red'),lwd=c(1.5,1.5))
#WMT:
query <- "SELECT tsymbol,prc,cfacshr,date
        FROM msf
        WHERE date BETWEEN '2002-01-01' AND '2009-12-31'
        AND tsymbol IN ('WMT')"
res1 <- dbGetQuery(con,query)
query <- "SELECT fyear, (csho*prcc_f)/ni AS pe, ni/csho AS eps
        FROM funda
        WHERE fyear >= 2002
        AND fyear <= 2010
        AND tic IN ('WMT')"
res2 <- dbGetQuery(con,query)
#Walmart from 2000 to 2010: stock price, EPS, P/E ratio:
par(mfrow=c(3,1))
plot(x=as.Date(res1$date),y=res1$prc,col="blue",type='l',
     xlab='date',ylab='price')
plot(x=res2$fyear,y=res2$eps,col='blue',type='l',
     xlab='date',ylab='EPS')
plot(x=res2$fyear,y=res2$pe,col='blue',type='l',
     xlab='date',ylab='P/E ratio')

query <- "SELECT tsymbol,date, (1+ret) AS ret
        FROM msf
        WHERE date BETWEEN '2010-12-01' AND '2013-12-31'
        AND tsymbol IN (SELECT tic
                        FROM funda
                        WHERE fyear = 2010
                        AND (sich < 6000 OR sich > 6999)
                        AND seq > 1000
                        AND ni/(prcc_f*csho) > .1
                        AND ni/(prcc_f*csho) IS NOT NULL
                        AND oancf/(csho*prcc_f) > 0.2
                        AND oancf/(csho*prcc_f) IS NOT NULL
                        AND fic = 'USA')
                        ORDER BY tsymbol, date"
result<-dbGetQuery(con,query)

result <- melt(result,id=c("tsymbol","date"),na.rm=TRUE)
result <- dcast(result, tsymbol~variable, prod)
result
mean(result$ret)

query <- "SELECT tsymbol,date, (1+ret) AS ret
        FROM msf
        WHERE date BETWEEN '2010-12-01' AND '2013-12-31'
        AND tsymbol IN (SELECT tic
                        FROM funda
                        WHERE fyear = 2010
                        AND (sich < 6000 OR sich > 6999)
                        AND seq > 1000
                        AND ni/(prcc_f*csho) > .15
                        AND oancf/(csho*prcc_f) > 0.25
                        AND fic = 'USA')
                        ORDER BY tsymbol, date"
result <- dbGetQuery(con,query)

result <- melt(result,id=c("tsymbol","date"),na.rm=TRUE)
dcast(result, date ~ tsymbol + variable)
result <- dcast(result, tsymbol ~ variable, prod)
#equally weighted portfolio return:
mean(result$ret)

query<-"SELECT tic FROM funda
        WHERE fyear = 2010
        AND (sich < 6000 OR sich > 6999)
        AND seq > 1000
        AND ni/(prcc_f*csho) > .15
        AND ni/(prcc_f*csho) IS NOT NULL
        AND oancf/(csho*prcc_f) > 0.25
        AND oancf/(csho*prcc_f) IS NOT NULL
        AND fic = 'USA'"
result <- dbGetQuery(con,query)
result
str(result)
library(quantmod)
getSymbols(result$tic, from = "2010-12-01", to = "2013-12-31")
MU <- MU[, "MU.Adjusted", drop=F]
WDC <- WDC[, "WDC.Adjusted", drop=F]
UFS <- UFS[, "UFS.Adjusted", drop=F]
par(mfrow=c(3,1))
plot(MU)
plot(WDC)
plot(UFS)

query<-"SELECT tic, ni/(prcc_f*csho) AS ep, ni/(csho*prcc_f + lt) AS roa
        FROM funda
        WHERE fyear = 2010
            AND (sich < 6000 OR sich > 6999)
            AND seq > 1000
            AND ep > .1
            AND ep IS NOT NULL
            AND roa > 0.1
            AND roa IS NOT NULL
            AND fic = 'USA'"
res<-dbGetQuery(con,query)
res

query <- "SELECT tic, lt/ni AS pay
          FROM funda
          WHERE fyear = 2010
              AND seq > 1000
              AND fic = 'USA'
              AND pay < 2
              AND pay > 0"
res <- dbGetQuery(con,query)

dbDisconnect(con)
