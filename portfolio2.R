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
start <- "2007-01-01"
end <- "2017-12-31"
wind <- "2008-01-01"

#Grab prices and vix and return Zoo object
getSymbols("^GSPC", from=start, to=end, src="yahoo", return.class = 'zoo')
getSymbols("^VIX", from=start, to=end, src="yahoo", return.class = 'zoo')

#Grab 3mo US Treasury Data or LIBOR
#US Treasury DGS3MO <- Missing Federal Holidays
getSymbols("DGS3MO", src = "FRED", return.class = 'zoo')

vol <- rollapply(data = (GSPC$GSPC.Adjusted/lag(GSPC$GSPC.Adjusted)-1), sd, width = 252)
vol <- na.locf(vol, fromLast = TRUE)
vol <- vol*sqrt(252)

#Merge VIX and Treasury data
volR <- merge(vol, DGS3MO)

#Federal holidays that are not also bank holidays do not
#have a treasury offered but do have a stock return. In 
#this case we used the 3m0 rate from the day before
DGS3MO <- na.locf(volR[,2], na.rm=TRUE)

#Merge VIX and Treasury data
volR <- merge(vol, DGS3MO, all = FALSE)
colnames(volR) <- c("vol","DGS3MO")

#Compute 60 and 120 day moving averages
move60 <- rollmean(GSPC$GSPC.Adjusted, k = 60, align = 'r')
move120 <- rollmean(GSPC$GSPC.Adjusted, k = 120, align = 'r')

#Combine prices, move60, and move120
data <- merge(GSPC$GSPC.Adjusted, move60, move120)
colnames(data) <- c("GSPC","Move60", "Move120")

#Trim data to Begin at 12-31-1990
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

#Merge VIX and Treasury data into the dataset
data <- merge(data, volR)

#Black Scholes Option Pricing Model
blackScholes <- function(type, s0, k, r, sigma, t, bigT){
  r <- r/100
  sigma <- sigma
  dPlus  <- (log(s0/k)+(r + sigma^2/2)*(bigT-t))/(sigma*sqrt(bigT-t))
  dMinus <- (log(s0/k)+(r - sigma^2/2)*(bigT-t))/(sigma*sqrt(bigT-t))
  if(type == 'c'){
    return(s0*pnorm(dPlus)-k*exp(-r*(bigT-t))*pnorm(dMinus))
  }
  return(k*exp(-r*(bigT-t))*pnorm(-dMinus)-s0*pnorm(-dPlus))
}

#Function for getting black scholes call and put
bsCall <- function(x){
  buyPrice  <- blackScholes('c', x[1,'GSPC'], x[1,'GSPC'], x[1,'DGS3MO'], x[1,'vol'], 0/365, 90/365)
  sellPrice <- blackScholes('c', x[2,'GSPC'], x[1,'GSPC'], x[2,'DGS3MO'], x[2,'vol'], 1/365, 90/365)
  return(c(buyPrice,sellPrice))
}

bsPut <- function(x){
  buyPrice  <- blackScholes('p', x[1,'GSPC'], x[1,'GSPC'], x[1,'DGS3MO'], x[1,'vol'], 0/365, 90/365)
  sellPrice <- blackScholes('p', x[2,'GSPC'], x[1,'GSPC'], x[2,'DGS3MO'], x[2,'vol'], 1/365, 90/365)
  return(c(buyPrice,sellPrice))
}

#Apply functions to data
callPrices <- rollapply(data, FUN = bsCall, width = 2, by.column = FALSE, align = 'r')
putPrices <- rollapply(data, FUN = bsPut, width = 2, by.column = FALSE, align = 'r')

#Rename Columns
colnames(callPrices) <- c("CallBuy","CallSell")
colnames(putPrices) <- c("PutBuy","PutSell")

#Merge prices with data
data <- merge(data, callPrices, putPrices)

#Function to determine daily return
returns2 <- function(x){
  if(x[1,'pos']){
    return(log(x[2,'CallSell'])-log(x[2,'CallBuy']))
  }
  return(log(x[2,'PutSell'])-log(x[2,'PutBuy']))
}

dollarReturns2 <- function(x){
  if(x[1,'pos']){
    return(x[2,'CallSell']-x[2,'CallBuy'])
  }
  return(x[2,'PutSell']-x[2,'PutBuy'])
}

data <- window(data, start = as.Date(wind))
return2 <- rollapply(data, FUN = returns2, width = 2, by.column = FALSE, align = 'r')
cumSum2 <- cumsum(na.omit(return2))
dollarReturn2 <- rollapply(data, FUN = dollarReturns2, width = 2, by.column = FALSE, align = 'r')
dollarCumSum2 <- cumsum(na.omit(dollarReturn2))

#Plot data to check correctness
plot(dollarCumSum2, xlab = 'Date', ylab = 'Return', main = 'Portfolio 2',type = 'l', lwd = 2, col = 4)
