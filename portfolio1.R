# IAQF Research Challenge
# Jack Sneeringer
# University of Washington

library(zoo)
library(tseries)
library(quantmod)

rm(list = ls())

#Suppress warnings
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

#Set dates
start <- "1989-07-01"
end <- "2017-12-31"
wind <- "1989-12-29"

#Grab prices and return Zoo object
getSymbols("^GSPC", from=start, to=end, src="yahoo", return.class = 'zoo')

#Compute 60 and 120 day moving averages
move60 <- rollmean(GSPC$GSPC.Adjusted, k = 60, align = 'r')
move120 <- rollmean(GSPC$GSPC.Adjusted, k = 120, align = 'r')

#Combine prices, move60, and move120
data <- merge(GSPC$GSPC.Adjusted, move60, move120)
colnames(data) <- c("GSPC","Move60", "Move120")

#Trim data to begin at 12-31-1990
data <- window(data, start = as.Date(wind))

#Function to determine position at the end of each trading day
longOrShort <- function(x){ return(x['Move60']>x['Move120']) }

#Apply function to each item in the dataset and then merge
pos <- rollapply(data, FUN =longOrShort, width = 1, by.column = FALSE)
data <- merge(data,pos)

#Function to calculate the number of times portfolio transitions
changePos <- function(x){ return(x[2,'pos'] != x[1,'pos']) }

#Apply function to each item in dataset and then merge
chngPos <- rollapply(data, FUN =changePos, width = 2, by.column = FALSE, align = 'r')
data <- merge(data,chngPos)

#Determine Value of holdings through time
logReturn <- function(x,mode = 'sp') {
  if(mode == 'mo' && !x[1,'pos']){
    return(-diff(log(x[,'GSPC'])))
  }
  return(diff(log(x[,'GSPC'])))
}

#Determine Value of holdings through time
dollarReturn <- function(x,mode = 'sp') {
  if(mode == 'mo' && !x[1,'pos']){
    return(-diff(x[,'GSPC']))
  }
  return(diff(x[,'GSPC']))
}

#Apply function
returns <- rollapply(data, FUN = logReturn, mode = 'mo', width = 2, by.column = F, align = 'r')
returnsSP <- rollapply(data, FUN = logReturn, width = 2, by.column = F, align = 'r')
dollarReturns <- rollapply(data, FUN = dollarReturn, mode = 'mo', width = 2, by.column = F, align = 'r')
dollarReturnsSP <- rollapply(data, FUN = dollarReturn, width = 2, by.column = F, align = 'r')
data <- merge(data,dollarReturns)

#DollarReturns
dollarCumSum <- cumsum(dollarReturns)
dollarCumSumSP <- cumsum(dollarReturnsSP)

plot <- merge(dollarCumSumSP, dollarCumSum)
colnames(plot) <- c("S&P 500", "Momentum")
plot <- window(plot, start = as.Date(wind))
plot(x = plot, xlab = 'Date', ylab = c('Return'), 
     main = 'Portfolio 1',lwd = 2, screens = c(1,1), col = c(2,4))
legend("bottomright", col = c(2,4), pch = '_', legend = colnames(plot))





