library(zoo)
library(tseries)
library(quantmod)
library(ggplot2)
library(gridExtra)
library(fBasics)
library(pracma)
library(PerformanceAnalytics)
# source("function.R")
rm(list = ls())

##Function:---------------------------------------------------------------
#Function to determine position at the end of each trading day
longOrShort <- function(x) { return(x['Move60'] > x['Move120']) }

#Black Scholes Option Pricing Model
blackScholes <- function(type, s0, k, r, sigma, t, bigT) {
  r <- r / 100
  dPlus <- (log(s0 / k) + (r + sigma ^ 2 / 2) * (bigT - t)) / (sigma * sqrt(bigT - t))
  dMinus <- (log(s0 / k) + (r - sigma ^ 2 / 2) * (bigT - t)) / (sigma * sqrt(bigT - t))
  if (type == 'c') {
    return(s0 * pnorm(dPlus) - k * exp(-r * (bigT - t)) * pnorm(dMinus))
  }
  return(k * exp(-r * (bigT - t)) * pnorm(-dMinus) - s0 * pnorm(-dPlus))
}

#Function for getting black scholes call and put
bsCall <- function(x) {
  buyPrice <- blackScholes('c', x[1, 'GSPC'], x[1, 'GSPC'], x[1, 'DGS3MO'], x[1, 'vol'], 0 / 365, 90 / 365)
  sellPrice <- blackScholes('c', x[2, 'GSPC'], x[1, 'GSPC'], x[2, 'DGS3MO'], x[2, 'vol'], 1 / 365, 90 / 365)
  return(c(buyPrice, sellPrice))
}

bsPut <- function(x) {
  buyPrice <- blackScholes('p', x[1, 'GSPC'], x[1, 'GSPC'], x[1, 'DGS3MO'], x[1, 'vol'], 0 / 365, 90 / 365)
  sellPrice <- blackScholes('p', x[2, 'GSPC'], x[1, 'GSPC'], x[2, 'DGS3MO'], x[2, 'vol'], 1 / 365, 90 / 365)
  return(c(buyPrice, sellPrice))
}

#Calculate returns of the portfolio 1:
logReturn <- function(x, mode = 'sp') {
  if (mode == 'mo' && !x[1, 'pos']) {
    return(-diff(log(x[, 'GSPC'])))
  }
  return(diff(log(x[, 'GSPC'])))
}

dollarReturn <- function(x, mode = 'sp') {
  if (mode == 'mo' && !x[1, 'pos']) {
    return(-diff(x[, 'GSPC']))
  }
  return(diff(x[, 'GSPC']))
}

#Calculate returns of the portfolio 2:
returns2 <- function(x) {
  if (x[1, 'pos']) {
    return(log(x[2, 'CallSell']) - log(x[2, 'CallBuy']))
  }
  return(log(x[2, 'PutSell']) - log(x[2, 'PutBuy']))
}

dollarReturns2 <- function(x) {
  if (x[1, 'pos']) {
    return(x[2, 'CallSell'] - x[2, 'CallBuy'])
  }
  return(x[2, 'PutSell'] - x[2, 'PutBuy'])
}

#Calculate returns of the portfolio 3:
returns3 <- function(x) {
  return(log(x['CallSell'] + x['PutSell']) - log(x['CallBuy'] + x['PutBuy']))
}
dollarReturns3 <- function(x) {
  return(x['CallSell'] + x['PutSell'] - x['CallBuy'] - x['PutBuy'])
}

#Day Count
day <- function(len, indices, startDate){
  new<-rep(0,len)
  exp<-rep(NA,len)
  zCount  <- zoo(new,indices)
  zLength <- zoo(exp,indices)
  date <- as.Date(startDate)
  idx <- 3
  zLength[3] <- 90
  for(i in 1:len){
    diff <- as.numeric(indices[i]-date)
    if(diff >= 90){
      adjIdx <- diff-90
      zCount[i-adjIdx] <- 0
      zCount[i] <- as.numeric(indices[i]-indices[i-adjIdx])
      zLength[idx+1] <- 180-diff
      date <- indices[i-adjIdx]
      idx <- i-adjIdx
    }
    else{
      zCount[i] <- diff
    }
  }
  return(merge(zCount,zLength))
}

#Strike of Current Straddle
strike <- function(x) {
  if (!x['dayCount']) {
    return(c(x['GSPC'],x['vol']))
  }
}

#for 90 day options we are holding
bsStrad <- function(x) {
  callBuy  <- blackScholes('c', x[1, 'GSPC'], x[1, 'k'], x[1, 'DGS3MO'], x[1, 'vol'], x[1, 'dayCount'] / 365, 90 / 365)
  callSell <- blackScholes('c', x[2, 'GSPC'], x[2, 'k'], x[2, 'DGS3MO'], x[2, 'vol'], x[2, 'dayCount'] / 365, 90 / 365)
  putBuy   <- blackScholes('p', x[1, 'GSPC'], x[1, 'k'], x[1, 'DGS3MO'], x[1, 'vol'], x[1, 'dayCount'] / 365, 90 / 365)
  putSell  <- blackScholes('p', x[2, 'GSPC'], x[2, 'k'], x[2, 'DGS3MO'], x[2, 'vol'], x[2, 'dayCount'] / 365, 90 / 365)
  hedgeBuy  <- blackScholes('c', x[1, 'GSPC'], x[1, 'GSPC']*1.1, x[1, 'DGS3MO'], x[1, 'vol'], 0 / 365, 90 / 365)
  hedgeSell <- blackScholes('c', x[2, 'GSPC'], x[1, 'GSPC']*1.1, x[2, 'DGS3MO'], x[2, 'vol'], 1 / 365, 90 / 365)
  return(c(callBuy + putBuy, callSell + putSell, hedgeBuy, hedgeSell))
}

#Function for calculating delta of black scholes option
deltaBS <- function(type, s0, k, r, sigma, t, bigT) {
  r <- r / 100
  dPlus <- (log(s0 / k) + (r + sigma ^ 2 / 2) * (bigT - t)) / (sigma * sqrt(bigT - t))
  if (type == 'c') {
    return(pnorm(dPlus))
  }
  return(-pnorm(-dPlus))
}

delta <- function(x) {
  dCallB   <- deltaBS('c', x['GSPC'], x['GSPC'], x['DGS3MO'], x['vol'], 0 / 365, 90 / 365)
  dPutB    <- deltaBS('p', x['GSPC'], x['GSPC'], x['DGS3MO'], x['vol'], 0 / 365, 90 / 365)
  dCallB90 <- deltaBS('c', x['GSPC'], x['k'], x['DGS3MO'], x['vol'], x['dayCount'] / 365, 90 / 365)
  dPutB90  <- deltaBS('p', x['GSPC'], x['k'], x['DGS3MO'], x['vol'], x['dayCount'] / 365, 90 / 365)
  dHedgeB  <- deltaBS('c', x['GSPC'], x['GSPC']*1.1, x['DGS3MO'], x['vol'], 0 / 365 , 90 / 365)
  return(c(dCallB + dPutB, dCallB90 + dPutB90, dHedgeB))
}

#Function for calculating gamma of black scholes option
gammaBS <- function(s0, k, r, sigma, t, bigT) {
  r <- r / 100
  dPlus <- (log(s0 / k) + (r + sigma ^ 2 / 2) * (bigT - t)) / (sigma * sqrt(bigT - t))
  phi <- exp(-dPlus^2/2)/sqrt(2*pi)
  return(phi/(s0*sigma*sqrt(bigT-t)))
}

gamma <- function(x) {
  gStradB <- 2*gammaBS(x['GSPC'], x['GSPC'], x['DGS3MO'], x['vol'], 0 / 365, 90 / 365)
  gStradB90 <- 2*gammaBS(x['GSPC'], x['k'], x['DGS3MO'], x['vol'], x['dayCount'] / 365, 90 / 365)
  gHedge <- gammaBS(x['GSPC'], x['GSPC']*1.1, x['DGS3MO'], x['vol'], 0 / 365, 90 / 365)
  return(c(gStradB, gStradB90, gHedge))
}
# 
# #Function for calculating gamma of black scholes option
# thetaBS <- function(type, s0, k, r, sigma, t, bigT) {
#   r <- r / 100
#   dPlus  <- (log(s0 / k) + (r + sigma ^ 2 / 2) * (bigT - t)) / (sigma * sqrt(bigT - t))
#   dMinus <- (log(s0 / k) + (r - sigma ^ 2 / 2) * (bigT - t)) / (sigma * sqrt(bigT - t))
#   phi <- exp(-dPlus^2/2)/sqrt(2*pi)
#   if(type == 'c'){
#     return((-s0*phi*sigma)/(2*sqrt(bigT-t))-r*k*exp(-r*(bigT-t))*pnorm(dMinus))
#   }
#   return((-s0*phi*sigma)/(2*sqrt(bigT-t))+r*k*exp(-r*(bigT-t))*pnorm(-dMinus))
# }
# 
# #Function for calculating theta of black scholes option
# theta <- function(x) {
#   tCallB   <- thetaBS('c',x['GSPC'], x['GSPC'], x['DGS3MO'], x['vol'], 0 / 365, 90 / 365)
#   tPutB    <- thetaBS('p',x['GSPC'], x['GSPC'], x['DGS3MO'], x['vol'], 0 / 365, 90 / 365)
#   tCallB90 <- thetaBS('c',x['GSPC'], x['k'], x['DGS3MO'], x['vol'], x['dayCount'] / 365, 90 / 365)
#   tPutB90  <- thetaBS('p',x['GSPC'], x['k'], x['DGS3MO'], x['vol'], x['dayCount'] / 365, 90 / 365)
#   tHedge   <- thetaBS('c',x['GSPC'], x['GSPC']*1.1, x['DGS3MO'], x['vol'], 0 / 365, 90 / 365)
#   return(c(tCallB+tPutB, tCallB90+tPutB90, tHedge))
# }

#Calculate Shares Needed
numShares <- function(x) {
  deltaPortBuy <- x[1, 'deltaStrad'] - x[1, 'deltaStrad90']
  deltaPortSell <- x[2, 'deltaStrad'] - x[2, 'deltaStrad90']
  new <- deltaPortSell - deltaPortBuy
  return(c(new, deltaPortBuy))
}

#Calculate Shares Needed
numSharesGamma <- function(x) {
  gammaPortBuy  <- (x[1, 'gammaStrad']-x[1, 'gammaStrad90'])/x[1,'gammaHedge']
  gammaPortSell <- (x[2, 'gammaStrad']-x[2, 'gammaStrad90'])/x[2,'gammaHedge']
  deltaPortBuy  <- x[1, 'deltaStrad'] - x[1, 'deltaStrad90']-gammaPortBuy*x[1,'deltaHedge']
  deltaPortSell <- x[2, 'deltaStrad'] - x[2, 'deltaStrad90']-gammaPortSell*x[2,'deltaHedge']
  newDelta <- deltaPortSell - deltaPortBuy
  newGamma <- gammaPortSell - gammaPortBuy
  return(c(newDelta, deltaPortBuy, newGamma, gammaPortBuy))
}

#Calculate returns of the portfolio 4:
dollarReturns4 <- function(x) {
  if (x[2, 'dayCount']) {
    start <- x[1, 'GSPC'] * x[2, 'totShares'] + x[2, 'stradBuy90']
    end <- x[2, 'GSPC'] * x[2, 'totShares'] + x[2, 'stradSell90']
    ret <- end - start
  }
  else {
    start <- x[1, 'GSPC'] * x[2, 'totShares'] + x[2, 'stradBuy90']
    end <- x[2, 'GSPC'] * x[2, 'totShares'] + abs(x[1, 'k'] - x[2, 'GSPC'])
    ret <- end - start
  }
  return(ret)
}

dollarReturns4Gamma <- function(x) {
  if (x[2, 'dayCount']) {
    start <- x[1, 'GSPC'] * x[2, 'totShares'] + x[2, 'stradBuy90'] + x[2,'hedgeBuy'] * x[2,'totGamma']
    end <- x[2, 'GSPC'] * x[2, 'totShares'] + x[2, 'stradSell90'] + x[2,'hedgeSell'] * x[2,'totGamma']
    ret <- end - start
  }
  else {
    start <- x[1, 'GSPC'] * x[2, 'totShares'] + x[2, 'stradBuy90'] + x[2,'hedgeBuy'] * x[2,'totGamma']
    end <- x[2, 'GSPC'] * x[2, 'totShares'] + abs(x[1, 'k'] - x[2, 'GSPC']) + x[2,'hedgeSell'] * x[2,'totGamma']
    ret <- end - start
  }
  return(ret)
}

##Risk Metrics Functions:
Normal.VaR <- function(p, mu, sigma)
{
  z <- qnorm(p)
  var <- sigma * z + mu
  return(var)
}

NP.VaR <- function(x, p)
{
  n <- length(x)
  x <- sort(x)
  N1 <- floor(n * p)
  if (N1 <= 0)
  {
    VaR <- -x[1]
  }
  else
  {
    N2 <- N1 + 1
    N <- n * p
    w1 <- N2 - N
    w2 <- N - N1
    VaR1 <- -x[N1]
    VaR2 <- -x[N2]
    VaR <- w1 * VaR1 + w2 * VaR2
  }
  return(VaR)
}

Normal.ES <- function(p, mu, sigma)
{
  z = qnorm(p)
  f = dnorm(z)
  ES = mu - sigma / p * f
  return(ES)
}

NP.CVaR <- function(x, p)
{
  n = length(x)
  x = sort(x)
  N1 = floor(n * p)
  if (N1 <= 0) CVaR = -x[1]
  else {
    N2 = N1 + 1
    N = n * p
    w1 = N2 - N
    w2 = N - N1
    CVaR1 = -mean(x[x <= x[N1]])
    CVaR2 = -mean(x[x <= x[N2]])
    CVaR = w1 * CVaR1 + w2 * CVaR2
  }
  return(CVaR)
}

###Code:---------------------------------------------------------------------------------------------------------------
#Suppress warnings
options("getSymbols.yahoo.warning" = FALSE)
options("getSymbols.warning4.0" = FALSE)

#Set dates
start <- "2005-01-01"
end <- "2017-02-25"
wind <- "2006-12-28"
newWind <- "2007-01-03"
newEnd <- "2017-01-01"

#Grab prices and return Zoo object
getSymbols("^GSPC", from = start, to = end, src = "yahoo", return.type = "zoo", return.class = 'zoo')
getSymbols("^VIX", from = start, to = end, src = "yahoo", return.class = 'zoo')
getSymbols("^VXV", from = start, to = end, src = "yahoo", return.class = 'zoo')

#Grab 3mo US Treasury Data or LIBOR
#US Treasury DGS3MO <- Missing Federal Holidays
getSymbols("DGS3MO", src = "FRED", return.class = 'zoo')

vol <- rollapply(data = diff(log(GSPC$GSPC.Adjusted)), sd, width = 63, align = 'r')
vol <- na.locf(vol, fromLast = TRUE)
vol <- vol * sqrt(252)

#vol <- (VXV$VXV.Adjusted-4.69899)/100

p <- merge((VXV$VXV.Adjusted)/100,vol)
p <- window(p,start="2007-01-01")
plot(p,screens = c(1,1), col = c(2,4), main = "VXV and 90-day Trailing Volatility", ylab = "Volatility",
     xlab = "Time")
legend("topright", c("VXV","Trailing"), pch="_",col = c(2,4))

voltemp <- as.vector(window(vol,start="2007-01-01", end="2017-12-31"))
vxv <- as.vector(window(VXV$VXV.Adjusted,start="2007-01-01", end="2017-12-31"))
volstd <- (voltemp-median(voltemp, na.rm=TRUE))/mad(voltemp, na.rm=TRUE)
vxvstd <- (vxv-median(vxv, na.rm=TRUE))/mad(vxv, na.rm=TRUE)
qqplot(vxvstd, volstd, main= "QQ-Plot of VXV and Realized Volatility", xlab="VXV",ylab="Realized 90-day Volatility")
qntlvol = quantile(volstd, probs = c(0.25,0.75), na.rm=TRUE, names=FALSE)
qntlvxv = quantile(vxvstd, probs = c(0.25,0.75), na.rm=TRUE, names=FALSE)
abline( lm( qntlvol ~ qntlvxv ), col = 2)

#Merge VIX and Treasury data
volR <- merge(vol, DGS3MO)

#Federal holidays that are not also bank holidays do not
#have a treasury offered but do have a stock return. In 
#this case we used the 3m0 rate from the day before
DGS3MO <- na.locf(volR[, 2], na.rm = TRUE)

#Merge VIX and Treasury data
volR <- merge(vol, DGS3MO, all = FALSE)
colnames(volR) <- c("vol", "DGS3MO")

#Compute 60 and 120 day moving averages
move60 <- rollmean(GSPC$GSPC.Adjusted, k = 60, align = 'r')
move120 <- rollmean(GSPC$GSPC.Adjusted, k = 120, align = 'r')

#Combine prices, move60, and move120
data <- merge(GSPC$GSPC.Adjusted, move60, move120)
colnames(data) <- c("GSPC", "Move60", "Move120")

#Trim data to begin at 12-31-1990
data <- window(data, start = as.Date(wind))

#Apply function to each item in the dataset and then merge
pos <- rollapply(data, FUN = longOrShort, width = 1, by.column = FALSE)
data <- merge(data, pos)

#Function to calculate the number of times portfolio transitions
changePos <- function(x) { return(x[2, 'pos'] != x[1, 'pos']) }

#Apply function to each item in dataset and then merge
chngPos <- rollapply(data, FUN = changePos, width = 2, by.column = FALSE, align = 'r')
data <- merge(data, chngPos, volR)

#Apply functions to data
callPrices <- rollapply(data, FUN = bsCall, width = 2, by.column = FALSE, align = 'r')
putPrices <- rollapply(data, FUN = bsPut, width = 2, by.column = FALSE, align = 'r')

#Rename Columns
colnames(callPrices) <- c("CallBuy", "CallSell")
colnames(putPrices) <- c("PutBuy", "PutSell")

#Merge prices with data
data <- merge(data, callPrices, putPrices)
data <- window(data, start = as.Date(wind))

dayCount <- day(dim(data)[1], index(data), newWind)
dayCount <- na.locf(dayCount)
colnames(dayCount) <- c("dayCount","exp")
data <- merge(data, dayCount)

#Apply function to each item in dataset and then merge
k <- rollapply(data, FUN = strike, width = 1, by.column = FALSE)
temp <- merge(data, k)
k <- na.locf(temp[,'GSPC.k'])
volHold <- na.locf(temp[,'vol.k'])
data <- merge(data, k, volHold)

#Apply functions to data
stradPrices <- rollapply(data, FUN = bsStrad, width = 2, by.column = FALSE, align = 'r')
delta <- rollapply(data, FUN = delta, width = 1, by.column = FALSE)
gamma <- rollapply(data, FUN = gamma, width = 1, by.column = FALSE)

#Rename Columns
colnames(stradPrices) <- c("stradBuy90", "stradSell90", "hedgeBuy", "hedgeSell")
colnames(delta) <- c("deltaStrad", "deltaStrad90", "deltaHedge")
colnames(gamma) <- c("gammaStrad", "gammaStrad90", "gammaHedge")

#Merge prices with data
#data <- merge(data, stradPrices, delta)
data <- merge(data, stradPrices, delta, gamma)

#shares <- rollapply(data, FUN = numShares, width = 2, by.column = FALSE, align = 'r')
#colnames(shares) <- c("newShares", "totShares")
shares <- rollapply(data, FUN = numSharesGamma, width = 2, by.column = FALSE, align = 'r')
colnames(shares) <- c("newShares", "totShares", "newGamma", "totGamma")
data <- merge(data, shares)

#data <- window(data, start = as.Date(newWind), end = as.Date(newEnd))

dollarReturnsSP <- rollapply(data, FUN = dollarReturn, width = 2, by.column = FALSE, align = 'r')
dollarReturn1 <- rollapply(data, FUN = dollarReturn, mode = 'mo', width = 2, by.column = F, align = 'r')
dollarReturn2 <- rollapply(data, FUN = dollarReturns2, width = 2, by.column = FALSE, align = 'r')
dollarReturn3 <- rollapply(data, FUN = dollarReturns3, width = 1, by.column = FALSE, align = 'r')
#dollarReturn4 <- rollapply(data, FUN = dollarReturns4, width = 2, by.column = FALSE, align = 'r')
dollarReturn4 <- rollapply(data, FUN = dollarReturns4Gamma, width = 2, by.column = FALSE, align = 'r')

data <- merge(data, dollarReturnsSP, dollarReturn1, dollarReturn2, dollarReturn3, dollarReturn4)

dollarCumSumSP <- cumsum(na.omit(dollarReturnsSP))
dollarCumSum1 <- cumsum(na.omit(dollarReturn1))
dollarCumSum2 <- cumsum(na.omit(dollarReturn2))
dollarCumSum3 <- cumsum(na.omit(dollarReturn3))
dollarCumSum4 <- cumsum(na.omit(dollarReturn4))

data <- merge(data, dollarCumSumSP, dollarCumSum1, dollarCumSum2, dollarCumSum3, dollarCumSum4)

##Summary Statistics:------------------------------------------------
sum_stat <- data.frame(data$dollarReturn1, data$dollarReturn2, data$dollarReturn3, data$dollarReturn4, data$dollarReturnsSP)
colnames(sum_stat) <- c("Portfolio1", "Portfolio2", "Portfolio3", "Portfolio4", "S&P500")
stats <- basicStats(sum_stat)[-c(1,2, 9:12),] #remove unneccessary output

## Calculating VaR, CVaR, maximum drawdown, duration of maximum drawdown:
mean.rtn <- apply(sum_stat, 2, mean, na.rm = TRUE)
var.rtn <- apply(sum_stat, 2, var, na.rm = TRUE)
sd.rtn <- apply(sum_stat, 2, sd, na.rm = TRUE)

d <- NP.VaR(na.omit(coredata(dollarReturn1)), 0.95)

alpha <- 0.95
VaRisk1 <- mean.rtn[1] + sqrt(var.rtn[1])*qnorm(1-alpha)
VaRisk2 <- mean.rtn[2] + sqrt(var.rtn[2])*qnorm(1-alpha)
VaRisk3 <- mean.rtn[3] + sqrt(var.rtn[3])*qnorm(1-alpha)
VaRisk4 <- mean.rtn[4] + sqrt(var.rtn[4])*qnorm(1-alpha)
VaRiskSP <- mean.rtn[5] + sqrt(var.rtn[5])*qnorm(1-alpha)
CVaR1 <- -(mean.rtn[1] + sqrt(var.rtn[1])*dnorm(qnorm(alpha))/(1-alpha))
CVaR2 <- -(mean.rtn[2] + sqrt(var.rtn[2])*dnorm(qnorm(alpha))/(1-alpha))
CVaR3 <- -(mean.rtn[3] + sqrt(var.rtn[3])*dnorm(qnorm(alpha))/(1-alpha))
CVaR4 <- -(mean.rtn[4] + sqrt(var.rtn[4])*dnorm(qnorm(alpha))/(1-alpha))
CVaRSP <- -(mean.rtn[5] + sqrt(var.rtn[5])*dnorm(qnorm(alpha))/(1-alpha))
risk <- rbind(cbind(VaRisk1, VaRisk2, VaRisk3, VaRisk4, VaRiskSP),
              cbind(CVaR1, CVaR2, CVaR3, CVaR4, CVaRSP))
colnames(risk) <- c("P1", "P2", "P3", "P4", "P5")
rownames(risk) <- c("Var", "CVaR")

##Change-------------------------------------------------------------------
dat <- data.frame(as.Date(time(data)), data)
colnames(dat)[1] <- "Date"

##Plot--------------------------------------------------------------------
plot1 <- ggplot(data = dat, aes(x = Date)) +
  geom_line(aes(y = GSPC, colour = 'S&P500'), size = 0.3) +
  geom_line(aes(y = Move60, colour = 'MA60'), size = 0.3) +
  geom_line(aes(y = Move120, colour = 'MA120'), size = 0.3) +
  theme(axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 9),
        legend.position = 'right',
        axis.title = element_text(size = 10), 
        # legend.key.size = unit(1, 'line'),
        legend.box.spacing = unit(0.5, 'mm')) +
  scale_x_date(date_labels = '%Y', breaks = scales::pretty_breaks(n = 5)) +
  scale_colour_manual("",
                      breaks = c('S&P500', 'MA60', 'MA120'),
                      values = c('darkgreen', 'red', 'grey60')) +
  ggtitle("") +
  ylab("S&P500 Index Price") +
  xlab("")

p1 <- ggplot(data = dat, aes(x = Date)) +
  geom_line(aes(y = dollarCumSumSP, colour = 'S&P500'), size = 0.1) +
  geom_line(aes(y = dollarCumSum1, colour= 'Portfolio1'), size = 0.3) +
  geom_line(aes(y = dollarCumSum2, colour = 'Portfolio2'), size = 0.3) +
  geom_line(aes(y = dollarCumSum3, colour = 'Portfolio3'), size = 0.3) +
  geom_line(aes(y = dollarCumSum4, colour = 'Portfolio4'), size = 0.3) +
  # theme_grey(base_size = 25) +
  theme(axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 9),
        legend.position = 'right',
        axis.title = element_text(size = 10), 
        # legend.key.size = unit(1, 'line'),
        legend.box.spacing = unit(0.5, 'mm')) +
  scale_x_date(date_labels = '%Y', breaks = scales::pretty_breaks(n = 5)) +
  scale_colour_manual("", breaks = c('S&P500', 'Portfolio1', 'Portfolio2', 'Portfolio3', 'Portfolio4'), 
                      values = c('blue', 'red', 'seagreen', 'goldenrod', 'grey50')) +
  ggtitle("") +
  ylab("Cumulative Returns") +
  xlab("")

p1.dollarRtn<- ggplot(dat, aes(x = Date, y = dollarReturn1)) +
  geom_col(aes(y = dollarReturn1, colour = "Portfolio1"), width = 0.0001) +
  ggtitle("Portfolio 1 Dollar Return") +
  labs(x = "", y = "Return") +
  scale_x_date(date_labels = '%Y', breaks = scales::pretty_breaks(n=5)) +
  scale_colour_manual("",
                      breaks = c('Portfolio 1', 'S&P500'),
                      values = alpha(c('darkblue', 'grey60'))) +
  theme(axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 9),
        legend.position = 'right',
        axis.title = element_text(size = 10),
        # legend.key.size = unit(1, 'line'),
        legend.box.spacing = unit(0.5, 'mm'))

p2.dollarRtn<- ggplot(dat, aes(x = Date, y = dollarReturn2)) + 
  geom_col(colour = "darkblue", width = 0.0001) +
  ggtitle("Portfolio 2 Dollar Return") +
  labs(x = "", y = "Return") + 
  scale_x_date(date_labels = '%Y', breaks = scales::pretty_breaks(n=5)) + 
  theme(axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 9),
        legend.position = 'right',
        axis.title = element_text(size = 10), 
        # legend.key.size = unit(1, 'line'),
        legend.box.spacing = unit(0.5, 'mm'))

p3.dollarRtn<- ggplot(dat, aes(x = Date, y = dollarReturn3)) + 
  geom_col(colour = "darkblue", width = 0.0001) +
  ggtitle("Portfolio 3 Dollar Return") +
  labs(x = "", y = "Return") + 
  scale_x_date(date_labels = '%Y', breaks = scales::pretty_breaks(n=5)) + 
  theme(axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 9),
        legend.position = 'right',
        axis.title = element_text(size = 10), 
        # legend.key.size = unit(1, 'line'),
        legend.box.spacing = unit(0.5, 'mm'))

p4.dollarRtn<- ggplot(dat, aes(x = Date, y = dollarReturn4)) + 
  geom_col(colour = "darkblue", width = 0.0001) +
  ggtitle("Portfolio 4 Dollar Return") +
  labs(x = "", y = "Return") + 
  scale_x_date(date_labels = '%Y', breaks = scales::pretty_breaks(n=5)) + 
  theme(axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 9),
        legend.position = 'right',
        axis.title = element_text(size = 10), 
        # legend.key.size = unit(1, 'line'),
        legend.box.spacing = unit(0.5, 'mm'))

#geom_density(adjust = 0.02) kernel density:
p1.hist <- ggplot(dat, aes(x = dollarReturn1)) + 
  geom_histogram(color = 'darkblue', fill = 'white', bins = 200) + 
  ggtitle("Histogram of Portfolio 1 Dollar Return") + 
  labs(x = "", y = "")

p2.hist <- ggplot(dat, aes(x = dollarReturn2)) +
  geom_histogram(color = 'darkblue', fill = 'white', bins = 200) +
  ggtitle("Histogram of Portfolio 2 Arithmetic Return") +
  labs(x = "", y = "")

p3.hist <- ggplot(dat, aes(x = dollarReturn3)) +
  geom_histogram(color = 'darkblue', fill = 'white', bins = 200) +
  ggtitle("Histogram of Portfolio 3 Dollar Return") +
  labs(x = "", y = "")

p4.hist <- ggplot(dat, aes(x = dollarReturn4)) +
  geom_histogram(color = 'darkblue', fill = 'white', bins = 200) +
  ggtitle("Histogram of Portfolio 4 Dollar Return") +
  labs(x = "", y = "")

sp.hist <- ggplot(dat, aes(x = dollarReturnsSP)) +
  geom_histogram(color = 'darkblue', fill = 'white', bins = 200) +
  ggtitle("Histogram of S&P500 Dollar Return")

p.hist <- ggplot(data = dat, aes(x = dollarReturnsSP)) +
  geom_boxplot(aes(y = dollarReturn1))

p1

#names <- c(rep('Portfolio 1', 2769),
#rep('Portfolio 2', 2769),
#rep('Portfolio 3', 2769),
#rep('Portfolio 4', 2769),
#rep('S&P500', 2769))
#values = c(coredata(data$dollarReturn1),
#coredata(data$dollarReturn2),
#coredata(data$dollarReturn3),
#coredata(data$dollarReturn4),
#coredata(data$dollarReturnsSP))

#bplot <- data.frame(names, values)
#ggplot(bplot, aes(x = names, y = values, fill = names)) +
#geom_boxplot() +
#theme(legend.position = "none") +
#ylab("Dollar Return") +
#xlab("")
