#Chapter III
13*choose(4,2)*choose(12,3)*4^3 / choose(52,5)
choose(13,2)*choose(4,2)^2*11*4 / choose(52,5)

#For figure where 1/x = exp(-x)
x = seq(0,.5,.01)
y1 = 1-x
y2 = exp(-x)
plot(x,y1,type="l",ylab="1/x,exp(-x)",col=4)
lines(x,y2,type="l",col=6)

S = c(1.3,1.2,1.3,1.4,1.5,1.4,1.3,1.4,1.5)
diffLogS = diff(log(S))
diffLogSmean = mean(diffLogS)
N = length(diffLogS)
histVol = sqrt(1/(N-1)*sum((diffLogS-diffLogSmean)^2))
annHistVol = histVol*sqrt(length(S))
annHistVol

library(RSQLite)
library(foreign)
setwd(paste(homeuser,"/FinAnalytics/ChapIII",sep=""))
funda <- read.dta("funda.dta")
msf   <- read.dta("msf.dta")
con   <- dbConnect(SQLite(),":memory:")
dbWriteTable(con,"funda",funda,overwrite=TRUE)
dbWriteTable(con,"msf",msf,overwrite=TRUE)
command <- "SELECT tsymbol,ret
            FROM msf
            WHERE date BETWEEN '2005-01-01' AND '2013-12-31'
                AND tsymbol IN ('AAPL','SPY')"
result<-dbGetQuery(con,command)
y<-result[result$tsymbol=='AAPL',]$ret
x<-result[result$tsymbol=='SPY',]$ret

cov(x,y)/var(x)
summary(lm(y~x+1))
shapiro.test(x)
shapiro.test(y)
plot(x,y)
