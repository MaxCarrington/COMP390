# AVERAGE TRUE RANGE INDICATOR
#
# Indicator that calculates the Average True Range of a given period 
# Given a range made up of a lookback periods:
# Choose the maximum out of -> High - Low, |High - Previous Close|, |Low - Previous Close| for
# each period - notice, absolute value are used.
# ATR is then the sum of the True ranges over the specific period divided by the number of periods 
# 14 day is typically used
#
calculateTrueRange <- function(currentSeries, lookback){
  highLowDiff <- 0
  highPrevClose <- 0
  lowPrevClose <- 0
  trueRanges <- c()
  # May need to change this to go from 14-1 rather than 1-14
  for(i in 1:lookbackSize){
    highLowDiff <- currentSeries[i]$High - currentSeries[i]$Low
    highPrevClose <- currentSeries[i]$High - currentSeries[i-1]$Close
    lowPrevClose <- currentSeries[i]$Low - currentSeries[i-1]$Close
    ranges <- c(highLowDiff,highPrevClose,lowPrevClose)
    #Append the maximum value out of the above variables to the true ranges
    trueRanges <- c(max(ranges), trueRanges) 
    
  }
  
  #Sums up the true ranges and divides by the lookback size
  averageTrueRange <- sum(trueRanges) / length(currentSeries)
  return(averageTrueRange)
}