#-------------------------------------------------------------------------------
# Performs a Variance Ratio Test
# Used to test the random walk hypothesis in a time series. The null hypothesis 
# is that the series is a random walk (in this case, the variance ratio will be 
# 1). However if there is a notable deviation from 1 the time series may be 
# mean reverting or trending. If the 
#-------------------------------------------------------------------------------

# Performs a variance ratio test on a given time series and returns the variance
if (!require(vrtest)) install.packages("vrtest")
  library(vrtest)
calculateHurstExponent <- function(series) {
  log_returns <- diff(log(series))
  log_returns <- na.omit(log_returns)  # Removing NAs
  hurst_result <- hurstexp(log_returns) # Assuming 'hurstexp' is the function from the package you are using
  return(hurst_result$He) #Maybe change $He to any of the different hurst values
}


#Call with:
#variance_ratio_results <- lapply(inSampleDataList, function(series){
  #result <- performVarianceRatioTest(series$Close)
#})
#print(variance_ratio_results)
#-------------------------------------------------------------------------------