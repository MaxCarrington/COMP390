# AVERAGE TRUE RANGE INDICATOR
#
# Indicator that calculates the Average True Range of a given period 
# Given a range made up of a lookback periods:
# Choose the maximum out of -> High - Low, |High - Previous Close|, |Low - Previous Close| for
# each period - notice, absolute value are used.
# ATR is then the sum of the True ranges over the specific period divided by the number of periods 
# 14 day is typically used

# Calculate True Range from a period in time
calculateTrueRange <- function(currentSeries, lookback, startRange = 1) {
  trueRanges <- numeric(lookback)
  for (i in (startRange + 1):length(lookback)) {
    highLowDiff <- currentSeries[i]$High - currentSeries[i]$Low
    highPrevClose <- currentSeries[i]$High - currentSeries[i - 1]$Close
    lowPrevClose <- currentSeries[i]$Low - currentSeries[i - 1]$Close
    
    trueRanges[i] <- max(abs(highLowDiff), abs(highPrevClose), abs(lowPrevClose))
  }

  #Sums up the true ranges and divides by the lookback size
  averageTrueRange <- sum(trueRanges) / length(currentSeries)
  return(averageTrueRange)
}

# Repeatedly calculates the Average True Range for a specified range of time over a given time series
calculateATRForRange <- function(series, numToCheck,lookback, rangeLength=0, rangeIncrease){
  currentSeriesATRs <- c()
  startRangeIndex <- 1
  for(i in 1:numToCheck){
    averageTrueRange <- calculateTrueRange(series, lookback, startRangeIndex)
    currentSeriesATRs <- c(currentSeriesATRs, averageTrueRange)
    startRangeIndex <- startRangeIndex + rangeIncrease
    lookback <- lookback + rangeIncrease
  }
  return(currentSeriesATRs)
}