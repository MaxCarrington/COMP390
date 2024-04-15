source('./Indicators/ADF_test.R')
#Momentus
#   - Calculate Correlation Coefficient, measures the strength and direction of a linear relationship between two variables
#   - Determine Optimal Look-Back and Holding Periods
#   - Find the pair of past and future periods that yield the highest positive correlation
#   - This helps in identifying the optimal look-back period (for generating signals) and holding period (for maintaining positions)
#   - Prefer Shorter Holding Periods for Better Sharpe Ratio
#   -  Use Hurst Exponent and Variance Ratio Test
#   - Combine Findings to Determine Trend

#Calculates the log returns of the series 
toLogReturns <- function(series) {
  logReturns <- diff(log(series))
  returnDates <- index(series)
  logReturns <- xts(logReturns, order.by = returnDates)
  return(logReturns)
}

#Checks if a time series is stationary, by performing an ADF Test on a series (Typically log returns)
checkStationarity <- function(series){
  significantLevel <- 0.05
  isStationary <- FALSE
  adfTest <- performADFTest(series$Close)
  if(adfTest$p.value < significantLevel){ #Ensure the ADF test is statistically significant.
    isStationary <- TRUE
  }
  return(isStationary)
}

#Calculate the correlation between past and future returns of given (rolling) windows of a time series 
#and the dates of each window

calculateRollingCorrelationsWithDates <- function(series, windowSize, significanceLevel = 0.05) {
  # Initial checks and adjustments
  totalLength <- length(series$Close)
  windowSize <- min(windowSize, totalLength / 2)  # Ensure windowSize is not more than half of total length
  
  correlations <- numeric()
  startDatePast <- numeric()
  startDateFuture <- numeric()
  
  # Loop through the time series with the adjusted window size
  for (i in 1:(totalLength - 2 * windowSize + 1)) {
    # Set up indexes for past and future windows
    pastStart <- i
    pastEnd <- i + windowSize - 1
    futureStart <- pastEnd + 1
    futureEnd <- futureStart + windowSize - 1
    
    # Get past and future return data
    pastReturns <- series$Close[pastStart:pastEnd]
    futureReturns <- series$Close[futureStart:futureEnd]
    
    # Ensure both past and future data sets are non-NA and non-empty
    if (length(pastReturns) == length(futureReturns) && length(pastReturns) > 0) {
      # Calculate the correlation between past and future returns
      testResults <- cor.test(pastReturns, futureReturns, method = "pearson")
      
      # Only add to results if p-value indicates the correlation is statistically significant
      if (testResults$p.value < significanceLevel) {
        correlations <- c(correlations, testResults$estimate)
        startDatePast <- c(startDatePast, index(series)[pastStart])
        startDateFuture <- c(startDateFuture, index(series)[futureStart])
      }
    }
  }
  
  return(list(correlation = correlations,
              startDatePast = startDatePast,
              startDateFuture = startDateFuture))
}


calcOptimalWindowSize <- function(series, windowSize=30, startWindowSize = 10, endWindowSize = 40, stepSize = 2, significanceLevel = 0.05) {
  
  bestCorrelation <- -1
  optimalWindowSize <- NA
  correlationResult <- list()
  for (windowSize in seq(startWindowSize, endWindowSize, by = stepSize)) {
    # Calculate rolling correlations with the current window size
    corrRes <- calculateRollingCorrelationsWithDates(series, windowSize, significanceLevel)
    
    # Find the highest correlation in this iteration
    maxCorrelation <- max(corrRes$correlation, na.rm = TRUE)
    if (maxCorrelation > bestCorrelation) {
      bestCorrelation <- maxCorrelation
      correlationResult <- corrRes
      optimalWindowSize <- windowSize
    }
  }
  return(list(lookback = optimalWindowSize, 
              correlations = correlationResult$correlation, 
              startDatePast = correlationResult$startDatePast, 
              startDateFuture = correlationResult$startDateFuture))
}

checkLenForMomentum <- function(lengthThresh, correlations){
  numPositives <- length(which(correlations$correlation > 0))
  numNegatives <- length(which(correlations$correlation < 0))
  if(numPositives >= (length(correlations$correlation) * lengthThresh)){
    return("Momentum")
  }
  else if(numNegatives >= (length(correlations$correlation) * lengthThresh)){
    return("Mean-Reversion")
  }
  else{
    return("None")
  }
}

statisticallySuitable <- function(suitableForStrat, logReturns){
  hurstExponent <- calculateHurstExponent(na.omit(logReturns)) #Calculate the hurst exponent of the series. If H < 0.5, the series is mean reverting, if H > 0.5, series is trending, if H =0.5, Random walk
  if(hurstExponent < 0.5 && suitableForStrat == "Mean-Reversion"){
    return("Mean-Reversion")
  } else if(hurstExponent > 0.5 && suitableForStrat == "Momentum"){
    return("Momentum")
  } else {
    return("None")
  }
}

