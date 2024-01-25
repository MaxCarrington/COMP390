# AVERAGE TRUE RANGE INDICATOR
#
# Indicator that calculates the Average True Range of a given period 
# Given a range made up of a lookback periods:
# Choose the maximum out of -> High - Low, |High - Previous Close|, |Low - Previous Close| for
# each period - notice, absolute value are used.
# ATR is then the sum of the True ranges over the specific period divided by the number of periods 
# 14 day is typically used

# Calculate True Range from a period in time
calculateTrueRange <- function(series, startRangeIndex, lookback) {
  endIndex <- startRangeIndex + lookback - 1
  # Ensure we do not exceed the length of the series
  trueRanges <- numeric(lookback)
  for (i in 1:lookback) {
    currentIndex <- startRangeIndex + i - 1
    highLowDiff <- series$High[currentIndex] - series$Low[currentIndex]
    highPrevClose <- series$High[currentIndex] - series$Close[currentIndex - 1]
    lowPrevClose <- series$Low[currentIndex] - series$Close[currentIndex - 1]
    
    trueRanges[i] <- max(highLowDiff, abs(highPrevClose), abs(lowPrevClose))
  }
  
  mean(trueRanges)
}

calculateATRForRange <- function(series, numToCheck, lookback, rangeIncrease = 0) {
  currentSeriesATRs <- numeric(numToCheck)
  startRangeIndex <- 1
  
  for (i in 1:numToCheck) {
    currentSeriesATRs[i] <- calculateTrueRange(series, startRangeIndex, lookback)
    startRangeIndex <- startRangeIndex + rangeIncrease
  }
  
  return(currentSeriesATRs)
}

