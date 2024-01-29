# AVERAGE TRUE RANGE INDICATOR
#
# Indicator that calculates the Average True Range of a given period 
# Given a range made up of a lookback periods:
# Choose the maximum out of -> High - Low, |High - Previous Close|, |Low - Previous Close| for
# each period - notice, absolute value is used.
# ATR is then the sum of the True ranges over the specific period divided by the number of periods 
# 14 days is typically used

# Calculate True Range for a single period
calculateTrueRange <- function(series, currentIndex) {
  highLowDiff <- series$High[currentIndex] - series$Low[currentIndex]
  highPrevClose <- abs(series$High[currentIndex] - series$Close[currentIndex - 1])
  lowPrevClose <- abs(series$Low[currentIndex] - series$Close[currentIndex - 1])
  
  maxRange <- max(highLowDiff, highPrevClose, lowPrevClose)
  return(maxRange)
}

calculateATRForRangeXTS <- function(series, lookback) {
  indicatorSize <- length(series$Close) / lookback
  startRangeIndex <- 1
  
  currentSeriesATRs <- numeric(indicatorSize)
  atrDates <- numeric(indicatorSize)
  
  for (i in 1:indicatorSize) {
    endIndex <- startRangeIndex + lookback - 1
    if (endIndex > nrow(series)) {
      endIndex <- nrow(series)
    }
    trueRanges <- numeric(lookback)
    for (j in 1:lookback) {
      currentIndex <- startRangeIndex + j - 1
      trueRanges[j] <- calculateTrueRange(series, currentIndex)
    }
    currentSeriesATRs[i] <- mean(trueRanges)
    atrDates[i] <- index(series)[endIndex]
    startRangeIndex <- startRangeIndex + lookback
  }
  
  atrXTS <- xts(currentSeriesATRs, order.by = as.Date(atrDates))
  return(atrXTS)
}
