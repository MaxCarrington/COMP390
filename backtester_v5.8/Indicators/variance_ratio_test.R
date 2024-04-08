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

performVarianceRatioTest <- function(series, lags = c(2, 3, 5, 10), significanceThreshold = 1.95) {
  
  varRatioSignificant <- FALSE
  significantLag <- NA
  significantStats <- list()
  
  for (k in lags) {
    vr_test_result <- Lo.Mac(series, k = k)
    # Extract M1 and M2 statistics
    m1 <- vr_test_result$Stats[1]
    m2 <- vr_test_result$Stats[2]
    # Check if either statistic is significant
    if (abs(m1) >= significanceThreshold || abs(m2) >= significanceThreshold) {
      varRatioSignificant <- TRUE
      significantLag <- k
      significantStats <- list(M1 = m1, M2 = m2)
      break  # Exit the loop if any lag shows a significant result
    }
  }
  
  if (varRatioSignificant) {
    return(list(Significant = TRUE, Lag = significantLag, Stats = significantStats))
  } else {
    return(list(Significant = FALSE, Lag = NA, Stats = NA))
  }
}


#-------------------------------------------------------------------------------