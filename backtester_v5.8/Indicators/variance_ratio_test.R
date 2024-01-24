#-------------------------------------------------------------------------------
# Performs a Variance Ratio Test
# Used to test the random walk hypothesis in a time series. The null hypothesis 
# is that the series is a random walk (in this case, the variance ratio will be 
# 1). However if there is a notable deviation from 1 the time series may be 
# mean reverting or trending.
#-------------------------------------------------------------------------------

# Performs a variance ratio test on a given time series and returns the variance
# ratio
performVarianceRatioTest <- function(timeSeries) {
  testResult <- vrtest::vratio(timeSeries)
  return(testResult)
}
#-------------------------------------------------------------------------------