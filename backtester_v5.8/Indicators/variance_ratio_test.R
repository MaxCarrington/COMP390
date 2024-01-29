#-------------------------------------------------------------------------------
# Performs a Variance Ratio Test
# Used to test the random walk hypothesis in a time series. The null hypothesis 
# is that the series is a random walk (in this case, the variance ratio will be 
# 1). However if there is a notable deviation from 1 the time series may be 
# mean reverting or trending. If the 
#-------------------------------------------------------------------------------

# Performs a variance ratio test on a given time series and returns the variance
if (!require(vrtest)) 
  install.packages("vrtest")
  library(vrtest)

# Variance Ratio Test Function
performVarianceRatioTest <- function(series, lags = c( 2, 3, 5, 10), threshold=1.95) {
  log_returns <- diff(log(series))
  log_returns <- na.omit(log_returns)  # Remove NAs
  vr_test_results <- list()
  
  for (k in lags) {
    vr_test_result <- Lo.Mac(log_returns, k = k)
    vr_test_results[[paste("Lag", k)]] <- vr_test_result
  }
  successfulResults <- list()
  for (result in vr_test_results) {
    if (abs(result$Stats[[1]]) >= threshold) {
      successfulResults <- c(successfulResults, result$Stats[[1]])
    }
    if (abs(result$Stats[[2]]) >= threshold) {
      successfulResults <- c(successfulResults, result$Stats[[2]])
    }
  }
  
  return(successfulResults)
}


#Call with:
#variance_ratio_results_close <- lapply(inSampleDataList, function(series){
  #result <- performVarianceRatioTest(series$Close)
#})
#variance_ratio_results_open <- lapply(inSampleDataList, function(series){
  #result <- performVarianceRatioTest(series$Open)
#})
#print(variance_ratio_results_close)

#-------------------------------------------------------------------------------