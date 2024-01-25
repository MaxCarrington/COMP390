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

logarithmicReturns <- function(currentDayClose, previousDayClose) {
  return(log(currentDayClose / previousDayClose))
}

dailyVolatility <- function(series, lookback) {
  if(nrow(series) < lookback) {
    stop("Not enough data for the specified lookback period")
  }
  closingPrices <- tail(series$Close, lookback)
  logReturns <- diff(log(closingPrices))
  return(sd(logReturns,na.rm=TRUE))
}

annualisation <- function(series, lookback) {
  dailyVol <- dailyVolatility(series, lookback)
  numTradeDaysInYear <- 252
  annualisedVol <- dailyVol * sqrt(numTradeDaysInYear)
  return(annualisedVol)
}

calculateVIXForRange <- function(series, numToCheck, lookback, rangeIncrease) {
  currentSeriesVIXs <- numeric(numToCheck)
  startRangeIndex <- 1
  
  for(i in 1:numToCheck) {
    endIndex <- startRangeIndex + lookback - 1
    
    # Check if the endIndex exceeds the series length
    if(endIndex > nrow(series)) {
      currentSeriesVIXs[i] <- NA
    } else {
      currentRange <- series[startRangeIndex:endIndex, ]
      currentSeriesVIXs[i] <- annualisation(currentRange, lookback)
    }
    
    # Increment startRangeIndex for the next period
    startRangeIndex <- startRangeIndex + rangeIncrease
  }
  
  return(currentSeriesVIXs)
}

