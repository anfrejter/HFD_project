# setting the working directory if needed
# setwd("...")

library(xts)
library(chron)
library(TTR)
library(tseries)
library(knitr) # for nicely looking tables in html files
library(kableExtra) # for even more nicely looking tables in html files
library(quantmod) # for PnL graphs

Sys.setlocale("LC_TIME", "English")
#######################Statistics########################################

# mySR function
mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 

myCalmarRatio <- function(x, # x = series of returns
                          # scale parameter = Nt
                          scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
  
} 

statistics <- function(gross, net, scale) {
  
  grossSR <- mySR(gross, scale)
  netSR <- mySR(net, scale)
  grossCR <- myCalmarRatio(gross, scale)
  netCR <- myCalmarRatio(net, scale)
  stat = netCR * max(0, log(abs(sum(net)/1000)))
  
  return(data.frame(
    grossSR = grossSR,
    netSR = netSR,
    grossCR = grossCR,
    netCR = netCR,
    stat=stat
  ))
}

#results <- statistics(data.group1.daily$pnl_gross.mom,data.group1.daily$pnl_net.mom,252)

##############################





