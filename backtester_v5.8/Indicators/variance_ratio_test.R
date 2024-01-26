#-------------------------------------------------------------------------------
# Performs a Variance Ratio Test
# Used to test the random walk hypothesis in a time series. The null hypothesis 
# is that the series is a random walk (in this case, the variance ratio will be 
# 1). However if there is a notable deviation from 1 the time series may be 
# mean reverting or trending. If the 
#-------------------------------------------------------------------------------

# Performs a variance ratio test on a given time series and returns the variance
performVarianceRatioTest <- function(series){
  log_returns <- diff(log(series))
  log_returns <- na.omit(log_returns)
  vr_test_result <- Lo.Mac(log_returns, k = 2) # 'k' is the lag parameter
}

#-------------------------------------------------------------------------------