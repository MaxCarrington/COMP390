#-------------------------------------------------------------------------------
# Calculates the Hurst exponent
# The Hurst exponent measures the degree of stationarity or trend of a time 
# series. For a price series exhibiting a geometric random walk, H = 0.5, but 
# for a mean reverting series, H < 0.5, and for a trending series, H > 0.5. As H 
# decreases toward zero, the price series is more mean reverting and as H 
# increases toward 1 the price series is increasingly trending.
#-------------------------------------------------------------------------------

# Calculates the Hurst exponent for a given time series and returns the Hurst 
# exponent value
if (!require(pracma)) 
  install.packages("pracma")
  library(pracma)

calculateHurstExponent <- function(timeSeries) {
  H <- hurstexp(timeSeries, display=FALSE)
  return(H$Hrs)
}
#-------------------------------------------------------------------------------