#R program to obtain the latest set of tickers:
# using https://www.nasdaq.com/screening/company-list.aspx

df <- read.csv("companylist.csv",stringsAsFactors = F)
dim(df)
df <- df[,c(1,2)]
#eliminate ETFs:
idxs <- grep("ETF",df[,2])
if(length(idxs) > 0) df <- df[-idxs,]
head(df)
write.table(df[order(df$Symbol),],file="NASDAQclean.txt",row.names=F,quote=F,sep='\t')
df <- read.csv("companylist(1).csv",stringsAsFactors = F)
dim(df)
df <- df[,c(1,2)]
#eliminate ETFs:
idxs <- grep("ETF",df[,2])
if(length(idxs) > 0) df <- df[-idxs,]
head(df)
write.table(df[order(df$Symbol),],file="NYSEclean.txt",row.names=F,quote=F,sep='\t')
df <- read.csv("companylist(2).csv",stringsAsFactors = F)
dim(df)
df <- df[,c(1,2)]
#eliminate ETFs:
idxs <- grep("ETF",df[,2])
if(length(idxs) > 0) df <- df[-idxs,]
head(df)
write.table(df[order(df$Symbol),],file="AMEXclean.txt",row.names=F,quote=F,sep='\t')
