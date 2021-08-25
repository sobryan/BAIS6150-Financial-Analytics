#hfrom ttps://stackoverflow.com/questions/49452906/getfinancials-quantmod-and-tq-get-tidy-quant-not-working

# getFinancials (quantmod) and tq_get (tidy quant) not working?

#I'm getting the same error in both quantmod and tinyquant for financials data. Can anyone see if
#this is reproducable? Is this a google finance server issue? None of the below functions have been
#working for me.I'm not sure if it's me or the server.
#    tq_get("AAPL", get= "financials")
#    [1] NA
#    Warning message:
#    x = 'AAPL', get = 'financials': Error in thead[x]:thead[x + 1]: NA/NaN 
#    argument
#and:
#    getFin("AAPL")
#    Error in thead[x]:thead[x + 1] : NA/NaN argument
#Can somebody help?

#Yes I get the same issue for the past couple of days as well. I think it may have to do with a
#change on the part of Google Finance. The site is now different and url as well.

#Hi @Joe I faced the same problem, because google change its page, so I wrote a function
#to get data from Yahoo Finance. Its output is similar to getFin. I hope it can help you.

scrapy_stocks <- function(stock){
    if ("rvest" %in% installed.packages()) {
            library(rvest)
    }else{
            install.packages("rvest")
            library(rvest)
    }
    for (i in 1:length(stock)) {
            tryCatch(
                    {
                            url <- "https://finance.yahoo.com/quote/"
                            url <- paste0(url,stock[i],"/financials?p=",stock[i])
                            wahis.session <- html_session(url)                                
                            p <-    wahis.session %>%
                                    html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
                                    html_table(fill = TRUE)
                            IS <- p[[1]]
                            colnames(IS) <- paste(IS[1,])
                            IS <- IS[-c(1,5,12,20,25),]
                            names_row <- paste(IS[,1])
                            IS <- IS[,-1]
                            IS <- apply(IS,2,function(x){gsub(",","",x)})
                            IS <- as.data.frame(apply(IS,2,as.numeric))
                            rownames(IS) <- paste(names_row)
                            temp1 <- IS
                            url <- "https://finance.yahoo.com/quote/"
                            url <- paste0(url,stock[i],"/balance-sheet?p=",stock[i])
                            wahis.session <- html_session(url)
                            p <-    wahis.session %>%
                                    html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
                                    html_table(fill = TRUE)
                            BS <- p[[1]]
                            colnames(BS) <- BS[1,]
                            BS <- BS[-c(1,2,17,28),]
                            names_row <- BS[,1]
                            BS <- BS[,-1] 
                            BS <- apply(BS,2,function(x){gsub(",","",x)})
                            BS <- as.data.frame(apply(BS,2,as.numeric))
                            rownames(BS) <- paste(names_row)
                            temp2 <- BS
                            url <- "https://finance.yahoo.com/quote/"
                            url <- paste0(url,stock[i],"/cash-flow?p=",stock[i])
                            wahis.session <- html_session(url)
                            p <-    wahis.session %>%
                                    html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
                                    html_table(fill = TRUE)
                            CF <- p[[1]]
                            colnames(CF) <- CF[1,]
                            CF <- CF[-c(1,3,11,16),]
                            names_row <- CF[,1]
                            CF <- CF[,-1] 
                            CF <- apply(CF,2,function(x){gsub(",","",x)})
                            CF <- as.data.frame(apply(CF,2,as.numeric))
                            rownames(CF) <- paste(names_row)
                            temp3 <- CF
                            assign(paste0(stock[i],'.f'),value = list(IS = temp1,BS = temp2,CF = temp3),envir = parent.frame())

                    },
                    error = function(cond){
                            message(stock[i], "Give error ",cond)
                    }
            )
    }
}
#You can call it as scrapy_stocks(c("AAPL","GOOGL")) and access its data as AAPL.f$IS,AAPL.f$BS or AAPL.f$CF.
scrapy_stocks(c("NVDA"))
NVDA.f$IS
netincvec <- NVDA.f$IS['Net Income',]
fwdnetincvec <- rev(netincvec)
#NVDA annual gross returns or growth in net income:
as.numeric(fwdnetincvec[2:4]/fwdnetincvec[1:3])

