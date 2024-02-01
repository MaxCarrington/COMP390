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
  # Calculate log returns
  logReturns <- diff(log(series), lag = 1)
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

#Calculates the correlation between past and future returns, using a window for the past returns and a rolling window for the future returns
calculateRollingCorrelations <- function(series, windowSize) {
  rollingCorrelations <- numeric(0)
  
  windowStart <- 1
  windowEnd <- windowSize
  futureWindowStart <- windowEnd + 1
  futureWindowEnd <- futureWindowStart + windowSize - 1
  
  for (i in 1:(floor((length(series$Close)/windowSize)/2))) {
    
    past_returns <- na.omit(toLogReturns(series[windowStart:windowEnd]$Close))
    future_returns <- na.omit(toLogReturns(series[futureWindowStart:futureWindowEnd]$Close))
    
    correlation <- cor(past_returns, future_returns)
    rollingCorrelations<- c(rollingCorrelations, correlation)
    
    windowStart <- windowStart + windowSize
    windowEnd <- windowEnd + windowSize
    futureWindowStart <- windowEnd + windowSize
    futureWindowEnd <- futureWindowStart + windowSize -1 
    }
  return(rollingCorrelations)
}

# Function to calculate p-values for correlation coefficients
calculateCorrelationPValues <- function(correlations, n) {
  # n is the number of observations used in each correlation calculation
  p_values <- numeric(length(correlations))
  for (i in seq_along(correlations)) {
    t_value <- correlations[i] * sqrt((n - 2) / (1 - correlations[i]^2))
    p_values[i] <- 2 * pt(-abs(t_value), df = n - 2)  # Two-tailed test
  }
  return(p_values)
}

# Function to adjust p-values for multiple comparisons (using Bonferroni correction as an example)
adjustPValues <- function(p_values) {
  adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
  return(adjusted_p_values)
}
identifySignificantSeries <- function(correlationsList, significanceLevel = 0.05) {
  significantSeries <- list()
  
  # Loop through each series in the correlations list
  for (i in seq_along(correlationsList)) {
    seriesData <- correlationsList[[i]]
    correlations <- seriesData$correlation
    pValues <- seriesData$p.value
    
    # Determine if the series has significant correlations
    significantIndices <- which(pValues < significanceLevel)
    
    # Filter for significant correlations
    significantCorrelations <- correlations[significantIndices]
    
    # Optional: Further filter based on correlation strength if needed
    # For example, you might want correlations stronger than a certain threshold
    
    # If there are any significant correlations, add to the significantSeries list
    if (length(significantIndices) > 0) {
      significantSeries[[length(significantSeries) + 1]] <- list(
        seriesIndex = i,
        significantCorrelations = significantCorrelations,
        significantIndices = significantIndices
      )
    }
  }
  
  return(significantSeries)
}
