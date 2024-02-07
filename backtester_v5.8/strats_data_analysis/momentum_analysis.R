source('./Indicators/ADF_test.R')
#Momentus
#   - Calculate Correlation Coefficient, measures the strength and direction of a linear relationship between two variables
#   - Determine Optimal Look-Back and Holding Periods
#   - Find the pair of past and future periods that yield the highest positive correlation
#   - This helps in identifying the optimal look-back period (for generating signals) and holding period (for maintaining positions)
#   - Prefer Shorter Holding Periods for Better Sharpe Ratio
#   -  Use Hurst Exponent and Variance Ratio Test
#   - Combine Findings to Determine Trend

toLogReturns <- function(series) {
    logReturns <- diff(log(series))
  # If series is an xts or zoo object, create an xts object for log returns
  if (inherits(series, "xts") || inherits(series, "zoo")) {
    # Exclude the first date from the index, as there's no return for the first date
    returnDates <- index(series)
    logReturns <- xts(logReturns, order.by = returnDates)
  }
  
  return(logReturns)
}
checkStationarity <- function(series){
  isStationary <- FALSE
  #logReturns <- na.omit(toLogReturns(series)) #Remove NA's, adf will not work with them
  #adfTest <- performADFTest(logReturns$Close)
  adfTest <- performADFTest(series$Close)
  if(adfTest$p.value < 0.05){
    isStationary <- TRUE
  }
  return(isStationary)
}

calculateRollingCorrelationsWithDates <- function(series, windowSize, significanceLevel = 0.05) {
  correlations <- c()
  startDatePast <- c()
  startDateFuture <- c()
  for (i in 1:(length(series$Close) - windowSize * 2 + 1)) {
    pastStart <- i
    pastEnd <- i + windowSize - 1
    futureStart <- pastEnd + 1
    futureEnd <- futureStart + windowSize - 1
    
    pastReturns <- na.omit(series$Close[pastStart:pastEnd])
    futureReturns <- na.omit(series$Close[futureStart:futureEnd])
    # Ensure equal lengths before calculating correlation
    if (length(pastReturns) == length(futureReturns) && length(pastReturns) > 0) {
      correlation <- cor(pastReturns, futureReturns)
      p_value <- calculatePValue(correlation, length(pastReturns))
      
      # Only add to results if p-value is below the significance level
      if (p_value < significanceLevel) {
        correlations <- c(correlations, correlation)
        startDatePast <- c(startDatePast, index(series)[pastStart])
        startDateFuture <- c(startDateFuture, index(series)[futureStart])
      }
    }
  }
  
  return(list(correlation=correlations,
              startDatePast=startDatePast,
              startDateFuture=startDateFuture))
}



calculatePValue <- function(correlation, n) {
  # Calculate the t-statistic
  t_statistic <- correlation * sqrt((n - 2) / (1 - correlation^2))
  # Calculate the p-value
  p_value <- 2 * pt(-abs(t_statistic), df = n - 2)
  return(p_value)
}

# Function to adjust p-values for multiple comparisons (using Bonferroni correction as an example)
adjustPValues <- function(p_values) {
  adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
  return(adjusted_p_values)
}
identifySignificantSeries <- function(correlations, significanceLevel = 0.05, index) {
    # Determine if the series has significant correlations
    significantIndices <- which(correlations$p.value < significanceLevel)
    
    # Filter for significant correlations
    significantCorrelations <- correlations$correlations[significantIndices]
    # Optional: Further filter based on correlation strength if needed
    # For example, you might want correlations stronger than a certain threshold
    
    # If there are any significant correlations, add to the significantSeries list
    if (length(significantIndices) > 0) {
      significantSeries[[length(significantSeries) + 1]] <- list(
        seriesIndex = index,
        significantCorrelations = significantCorrelations,
        significantIndices = significantIndices
      )
    }
  
  return(significantSeries)
}
findOptimalLookback <- function(correlations){
  # Determine the range of lookback periods explored
  lookbackPeriods <- unique(correlations$optimalLookbackPeriod)
  
  # Initialize variables to store the best lookback period and its average correlation
  bestLookbackPeriod <- NA
  highestAvgCorrelation <- -Inf # Start with the lowest possible correlation
  
  # Iterate through each lookback period to calculate its average positive correlation
  for (lookback in lookbackPeriods) {
    # Indices of correlations that used this lookback period
    indices <- which(correlations$optimalLookbackPeriod == lookback & correlations$correlation > 0)
    
    # Calculate the average correlation for this lookback period
    avgCorrelation <- mean(correlations$correlation[indices])
    
    # Update the best lookback period if this one has a higher average correlation
    if(avgCorrelation > highestAvgCorrelation) {
      bestLookbackPeriod <- lookback
      highestAvgCorrelation <- avgCorrelation
    }
  }
  
  return(list(optimalLookbackPeriod = bestLookbackPeriod, avgCorrelation = highestAvgCorrelation))
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

statisticallySuitable <- function(suitableForStrat, hurstExp, vratio){
  if (any(is.na(vratio$Stats))) {
    return("None")
  }
  else if(hurstExp < 0.5 && vratio$Stats$M2 < 1 && suitableForStrat == "Mean-Reversion"){
    return("Mean-Reversion")
  }else if(hurstExp > 0.5 && vratio$Stats$M2 > 1 && suitableForStrat == "Momentum"){
    return("Momentum")
  }
  else{
    return("None")
  }
}
