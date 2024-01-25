# VOLATILITY INDEX INDICATOR
#
# File has 3 functions:
#
# Logarithmic returns: Computes the logarithmic differences of the daily average price for 
# two consecutive trading days, by taking the logarithm of the ratio of the closing
# prices. Uses the natural logarithm of the current day / previous day: ln(Pt/Pt-1)
#
# Daily Volatility: Calculates the daily volatility by taking the standard deviation of the 
# logarithmic returns over a given period (lookback paramater)
#
# Annualisation: Multiply the daily volatility by the square root of the number of trading 
# days in a year to annualise the volatility.

logatithmicReturns <- function(currentDayClose, previousDayClose){
  return(log(currentDayClose, previousDayClose))
}

dailyVolatility <- function(series, lookback, startRange=1){
  logReturns <- c()
  for(i in (startRange+1):lookback){
    logReturns <- c(logReturns, logatithmicReturns(prices[i]$Close, prices[i-1]$Close))
  }
  meanLogReturn <- (mean(logReturns))
  varianceSum <- 0
  for(i in 1:length(logReturns)){
    varianceSum <- varianceSum + (logReturns[i] - meanLogReturn)**2
  }
  volatilityIndex <- sqrt((varianceSum/(lookback-1)))
  return(volatilityIndex)
}

annualisation <- function(series, lookback, startRange=1){
  dailyVol <- dailyVolatility(prices, lookback, startRange) 
  numTradeDayInYear <- 252
  annualised <- dailyVol * sqrt(numTradeDayInYear)
  return (annualised)
}
# Repeatedly calculates the VIX for a specified range of time over a given time series
calculateVIXForRange <- function(series, numToCheck,lookback, rangeLength=0, rangeIncrease){
  currentSeriesVIXs <- c()
  startRangeIndex <- 1
  for(i in 1:numToCheck){
    volatilityIndex <- annualisation(series, lookback, startRangeIndex)
    currentSeriesVIXs <- c(currentSeriesVIXs, volatilityIndex)
    startRangeIndex <- startRangeIndex + rangeIncrease
    lookback <- lookback + rangeIncrease
  }
  return(currentSeriesVIXs)
}
