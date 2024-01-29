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

# Function to calculate the VIX for a given period

dailyVolatility <- function(series, lookback) {
  if(nrow(series) < lookback) {
    stop("Not enough data for the specified lookback period")
  }
closingPrices <- tail(series$Close, lookback)
logReturns <- diff(log(closingPrices))
return(sd(logReturns,na.rm=TRUE))
}
calculateVIXForPeriod <- function(series, startRangeIndex, lookback) {
  endIndex <- startRangeIndex + lookback - 1
  if (endIndex > nrow(series)) {
    endIndex <- nrow(series)
  }
  currentRange <- series[startRangeIndex:endIndex, ]
  dailyVol <- dailyVolatility(currentRange, lookback)
  numTradeDaysInYear <- 356
  annualisedVol <- dailyVol * sqrt(numTradeDaysInYear)
  return(annualisedVol)
}

# Function to create an XTS object of VIX values for the entire series
calculateVIXForRangeXTS <- function(series, lookback) {
  indicatorSize <- length(series$Close) / lookback
  startRangeIndex <- 1
  currentSeriesVIXs <- numeric(indicatorSize)
  vixDates <- numeric(indicatorSize)
  
  for (i in 1:indicatorSize) {
    endIndex <- startRangeIndex + lookback - 1
    # Ensure endIndex does not exceed the length of the series
    if (endIndex > nrow(series)) {
      endIndex <- nrow(series)
    }
    currentSeriesVIXs[i] <- calculateVIXForPeriod(series, startRangeIndex, endIndex - startRangeIndex + 1)
    vixDates[i] <- index(series)[endIndex]
    startRangeIndex <- startRangeIndex + lookback
  }
  
  vixXTS <- xts(currentSeriesVIXs, order.by = as.Date(vixDates))
  return(vixXTS)
}



