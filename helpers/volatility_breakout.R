# setting the working directory if needed
# setwd("...")
###########Dla NQ################################
library(xts)
library(chron)
library(TTR)
library(tseries)
library(knitr) # for nicely looking tables in html files
library(kableExtra) # for even more nicely looking tables in html files
library(quantmod) # for PnL graphs
library(caTools)
install.packages("lubridate")
library(lubridate)
install.packages("scales") # for rescale()
library(scales)
library(ggplot2)
library(RColorBrewer)

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_mySR.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")

#
# lets change the LC_TIME option to English
Sys.setlocale("LC_TIME", "English")

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

} # end of definition



# lets define the system time zone as America/New_York (used in the data)
Sys.setenv(TZ = 'America/New_York')

# do it simply in a loop on quarters
results_list <- list()

for (selected_quarter in c("2022_Q1", "2022_Q3", "2022_Q4",
                           "2023_Q2", "2023_Q4",
                           "2024_Q1", "2024_Q2")) {

  message(selected_quarter)

  # loading the data for a selected quarter from a subdirectory "data""

  filename_ <- paste0("data/data1_", selected_quarter, ".RData")

  load(filename_)

  # create index of times for this quarter

  data.group1 <- get(paste0("data1_", selected_quarter))

  times_ <- substr(index(data.group1), 12, 19)

  # the following common assumptions were defined:
  # 1.	do not use in calculations the data from the first
  # and last 15 minutes of the session (9:31--9:45 and 15:46--16:00)
  # – put missing values there,

  # lets put missing values for these periods
  # we don't have weekends included in the data

  data.group1["T09:31/T09:45",] <- NA
  data.group1["T15:46/T16:00",] <-NA


  pos_flat <- xts(rep(0, nrow(data.group1)),
                  index(data.group1))

  pos_flat["T15:46/T9:45"] <- 1
  #########################################################################
  for(signalEMA in c(10, 15, 20, 30, 45)) {
    for(slowEMA in c(60, 90, 120, 150, 180)) {
      for(volat.sd in c(60, 90, 120)) {
        for(m_ in c(1, 1.5, 2, 2.5, 3)) {

          print(paste("signalEMA = ", signalEMA,
                      ", slowEMA = ", slowEMA,
                      ", volat.sd = ", volat.sd,
                      ", m_ = ", m_, sep = ""))

          # calculating elements of the strategy

          # here calculation on coredata() makes a difference
          signalEMA.values <- EMA(na.locf(data.group1$NQ, na.rm = FALSE),
                                  signalEMA)
          slowEMA.values <- EMA(na.locf(data.group1$NQ, na.rm = FALSE),
                                slowEMA)
          volat.sd.values <- runsd(na.locf(data.group1$NQ, na.rm = FALSE),
                                   volat.sd,
                                   endrule = "NA",
                                   align = "right")

          # put missing values whenever the original price is missing
          signalEMA.values[is.na(data.group1$NQ)] <- NA
          slowEMA.values[is.na(data.group1$NQ)] <- NA
          volat.sd.values[is.na(data.group1$NQ)] <- NA

          # position for momentum strategy
          pos.mom <- positionVB_new(signal = signalEMA.values,
                                    lower = slowEMA.values - m_ * volat.sd.values,
                                    upper = slowEMA.values + m_ * volat.sd.values,
                                    pos_flat = coredata(pos_flat),
                                    strategy = "mom" # important !!!
          )

          # position for mean-rev strategy is just a reverse of pos.mom
          pos.mr <- (-pos.mom)

          # gross pnl
          pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(data.group1$NQ)),
                                  0, pos.mom * diff.xts(data.group1$NQ) * 20 # point value for E6
          )
          pnl.gross.mr <- (-pnl.gross.mom)

          # nr of transactions - the same for mom and mr
          ntrans <- abs(diff.xts(pos.mom))
          ntrans[is.na(ntrans)] <- 0

          # net pnl
          pnl.net.mom <- pnl.gross.mom - ntrans * 12
          pnl.net.mr <- pnl.gross.mr - ntrans * 12

          # aggregate to daily
          ends_ <- endpoints(data.group1, "days")

          pnl.gross.mom.d <- period.apply(pnl.gross.mom,
                                          INDEX = ends_,
                                          FUN = function(x) sum(x, na.rm = TRUE))
          pnl.gross.mr.d <- period.apply(pnl.gross.mr,
                                         INDEX=ends_,
                                         FUN = function(x) sum(x, na.rm = TRUE))
          pnl.net.mom.d <- period.apply(pnl.net.mom,
                                        INDEX = ends_,
                                        FUN = function(x) sum(x, na.rm = TRUE))
          pnl.net.mr.d <- period.apply(pnl.net.mr,
                                       INDEX = ends_,
                                       FUN = function(x) sum(x, na.rm = TRUE))
          ntrans.d <- period.apply(ntrans,
                                   INDEX = ends_,
                                   FUN = function(x) sum(x, na.rm = TRUE))

          # calculate summary measures
          gross.SR.mom <- mySR(pnl.gross.mom.d,
                               scale = 252)
          gross.SR.mr <- mySR(pnl.gross.mr.d,
                              scale = 252)
          net.SR.mom <- mySR(pnl.net.mom.d,
                             scale = 252)
          net.SR.mr <- mySR(pnl.net.mr.d,
                            scale = 252)
          gross.CR.mom <- myCalmarRatio(pnl.gross.mom.d,
                                        scale = 252)
          gross.CR.mr <- myCalmarRatio(pnl.gross.mr.d,
                                       scale = 252)
          net.CR.mom  <-  myCalmarRatio(pnl.net.mom.d,
                                        scale = 252)
          net.CR.mr  <-  myCalmarRatio(pnl.net.mr.d,
                                       scale = 252)

          gross.PnL.mom <- sum(pnl.gross.mom.d,
                               na.rm = TRUE)
          gross.PnL.mr <- sum(pnl.gross.mr.d,
                              na.rm = TRUE)
          net.PnL.mom <- sum(pnl.net.mom.d,
                             na.rm = TRUE)
          net.PnL.mr <- sum(pnl.net.mr.d,
                            na.rm = TRUE)


          stat.mr = net.CR.mr * max(0, log(abs(net.PnL.mr/1000)))
          stat.mom = net.CR.mom * max(0, log(abs(net.PnL.mom/1000)))

          av.daily.ntrans <- mean(ntrans.d[wday(ntrans.d) != 7],
                                  na.rm = TRUE)

          # summary of a particular strategy
          quarter_stats <- data.frame(quarter = selected_quarter,
                                 signalEMA = signalEMA,
                                 slowMA = slowEMA,
                                 volat.sd = volat.sd,
                                 m = m_,
                                 gross.SR.mom,
                                 gross.SR.mr,
                                 net.SR.mom,
                                 net.SR.mr,
                                 gross.PnL.mom,
                                 gross.PnL.mr,
                                 net.PnL.mom,
                                 net.PnL.mr,
                                 av.daily.ntrans,
                                 stat.mr,
                                 stat.mom,
                                 stringsAsFactors = FALSE
          )

          # collect summaries for all quarters
          results_list[[length(results_list) + 1]] <- quarter_stats

        } # end of loop for m_
      } # end of loop for volatility
    } # end of loop for slowEMA
  } # end of loop for signal


}

quarter_stats.all.group1 <- do.call(rbind, results_list)

# Write the combined results to a CSV file
write.csv(quarter_stats.all.group1,
          "quarter_stats.all.group1.csv",
          row.names = FALSE)


