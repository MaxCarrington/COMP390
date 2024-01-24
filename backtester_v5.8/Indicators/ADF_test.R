#-------------------------------------------------------------------------------
# Performs an Augmented Dickey fuller test
# Determines if a time series is stationary by testing for a unit root (a null
# hypothesis). If there is a unit root, then the time series may be 
# non-stationary. Alternatively if there is no unit root, this is indicative of
# stationarity.
#-------------------------------------------------------------------------------

# Performs an ADF test on a given time series
performADFTest <- function(timeSeries) {
  testResult <- adf.test(timeSeries, alternative = "stationary")
  return(testResult)
}

#-------------------------------------------------------------------------------