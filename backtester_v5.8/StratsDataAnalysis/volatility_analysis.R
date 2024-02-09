#-------------------------------------------------------------------------------
# - Volatility Analysis
#   - Calculate the ATR, VIX and std of log returns  
#   - Normalise the ATR and VIX indicators, using the Z-score
#   - Create a combined volatility score based on the normalised values
#   - Define volatility thresholds
#   - Analyse the combined score over time to identify perios of high/low volatility
#   - correlation Analysis: Explore the correlation between ATR and VIX
#-------------------------------------------------------------------------------
source('./Indicators/average_true_range.R')
source('./Indicators/volatility_index.R')
source('./Plot/plot_volatility.R')

analysePeriodVol <- function(series, lookback, monthly=FALSE){
  
  stdDev <- stdDevRollingWindow(series$Close, lookback)
  standardisedATRs <- zScoreStandardisation(calculateATRForRangeXTS(series, lookback))
  standardisedVIXs <- zScoreStandardisation(calculateVIXForRangeXTS(series, lookback))
  
  combinedATRVIX <- stdATRandVIX(standardisedATRs, standardisedVIXs)
  volClassifications <- classifyVolatility(combinedATRVIX)
  if(monthly && lookback == 7){
    
    weeksInMonth <- 4
    numberOfMonths <- floor(length(series$Close) / lookback) / weeksInMonth
    endOfMonthIndex <- weeksInMonth
    startOfMonthIndex <- 1
    monthlyVol <- c()
    
    for(i in 1:numberOfMonths){
      monthVol <- determineSeriesVolatility(volClassifications[startOfMonthIndex:endOfMonthIndex])
      monthlyVol <- c(monthlyVol, monthVol)
      startOfMonthIndex <- endOfMonthIndex + 1
      endOfMonthIndex <- endOfMonthIndex + weeksInMonth
    }
    
    seriesVol <- analyseSeriesVol(monthlyVol)
    volClassifications <- monthlyVol
    
  }else{
    seriesVol <- analyseSeriesVol(volClassifications)
  }
  return(list(PeriodVol = volClassifications, seriesVol = seriesVol, stdDev = stdDev))
}

analyseSeriesVol <- function(monthlyVolatility){
  nonVolatile = "Non-Volatile"
  volatile = "Volatile"
  highlyVolatile = "Highly Volatile"
  
  frequencyTable <- table(monthlyVolatility)
  numNonVolatile <- frequencyTable[nonVolatile]
  numVolatile <- frequencyTable[volatile]
  numHighVolatile <- frequencyTable[highlyVolatile]
  totalScores <- length(monthlyVolatility)
  
  # If 'High' or 'Highly Volatile' are not in the table, assign them a value of 0
  if(is.na(numNonVolatile)) numNonVolatile <- 0
  if(is.na(numVolatile)) numVolatile <- 0
  if(is.na(numHighVolatile)) numHighVolatile <- 0
  
  if(numNonVolatile >= totalScores*0.95){
    return(nonVolatile)
  }
  else if((numVolatile >= totalScores*0.20 && numVolatile <= totalScores*0.50) && numHighVolatile <= totalScores*0.10){
    return(volatile)
  }
  else{
    return(highlyVolatile)
  }
}


determineSeriesVolatility <- function(classifiedVolatility) {
  # Count the frequency of each volatility category
  frequencyTable <- table(classifiedVolatility)
  
  # Retrieve the number of 'High' and 'Highly Volatile' periods
  numHigh <- frequencyTable["High"]
  numHighlyVolatile <- frequencyTable["Highly Volatile"]
  
  # If 'High' or 'Highly Volatile' are not in the table, assign them a value of 0
  if(is.na(numHigh)) numHigh <- 0
  if(is.na(numHighlyVolatile)) numHighlyVolatile <- 0
  
  # Determine overall volatility based on the counts
  if (numHighlyVolatile >= 2 || numHigh > 2) {
    return("Highly Volatile")
  } else if (numHigh >= 2) {
    return("Volatile")
  } else {
    return("Non-Volatile")
  }
}



# Function to classify volatility periods based on percentiles and standard deviations
classifyVolatility <- function(volatilityScores) {
  meanScore <- mean(volatilityScores, na.rm = TRUE)
  sdScore <- sd(volatilityScores, na.rm = TRUE)
  
  # Define thresholds based on percentiles and standard deviations
  highVolPercentileThreshold <- quantile(volatilityScores, 0.7) # Top 30%
  highPercentileThreshold <- quantile(volatilityScores, 0.6)    # Top 40%
  
  highVolSDThreshold <- meanScore + 1.5 * sdScore # Threshold for "Highly Volatile"
  highSDThreshold <- meanScore + sdScore          # Threshold for "High"
  
  classifiedScores <- ifelse(volatilityScores > highVolPercentileThreshold & volatilityScores > highVolSDThreshold, "Highly Volatile",
                             ifelse(volatilityScores > highPercentileThreshold & volatilityScores > highSDThreshold, "High",
                                    ifelse(volatilityScores > meanScore, "Medium", "Low")))
  return(classifiedScores)
}

#Calculates volatility threshold values for a given indicator based on the current time series to determine times of high, low, medium volatility
#Not implemented currently
volThreshs <- function(indicator){
  volatilityPeriods <- c()
  avgVol <- mean(indicator)
  for(i in seq_along(indicator)){
    if((indicator[i] < avgVol* 1.25) && (indicator[i] > avgVol*0.75)){
      volatilityPeriods <- c(volatilityPeriods, "Medium")
    }
    else if(indicator[i] > avgVol*2){
      volatilityPeriods <- c(volatilityPeriods, "Very High")
    }
    else if(indicator[i] > avgVol*1.25){
      volatilityPeriods <- c(volatilityPeriods, "High")
    }
    else{
      volatilityPeriods <- c(volatilityPeriods, "Low")
    }
  }
  return(volatilityPeriods)
}

#Compute mean and standard deviation for each list, and apply the zScore forumla 
zScoreStandardisation <- function(indicatorValues){
  mean <- mean(indicatorValues, na.rm = TRUE)
  stdDev <- stats::sd(indicatorValues, na.rm = TRUE)
  zScore <- (indicatorValues - mean) /stdDev
  return(zScore)
}

stdATRandVIX <- function(normATRs, normVIXs) {
  if (!inherits(normATRs, "xts") || !inherits(normVIXs, "xts")) {
    stop("Both normATRs and normVIXs must be xts objects.")
  }
  
  # Ensure both xts objects have the same index (dates)
  if (!all(index(normATRs) == index(normVIXs))) {
    stop("The indices (dates) of normATRs and normVIXs do not match.")
  }
  
  # Average and combine both ATR and VIX
  combinedScores <- (normATRs + normVIXs) / 2
  
  return(combinedScores)
}

# Calculate the standard deviation of returns over a rolling window
stdDevRollingWindow <- function(series, lookback){
  
  # Calculate returns, ensuring no log of non-positive numbers
  returns <- diff(log(ifelse(series > 0, series, NA)), lag = 1)
  
  # Calculate rolling standard deviation, handling NA values
  rollingStdDev <- runSD(returns, n = lookback)
  return(rollingStdDev)
}

# Note: Before using the function, ensure your 'series' object is correctly structured,
# with a 'Close' column for prices and a 'Date' column or index for xts objects.



# Example usage:
# Assuming 'series' is your data frame or xts object with 'Close' prices and 'Date' (for data frames)
# lookbackPeriod <- 20
# stdDevRollingWindowAndPlot(series, lookbackPeriod)

#Call with:
# Loop through each time series and generate a PDF plot
#monthlyVolatility <- list()
#for (i in seq_along(inSampleDataList)) {
#  series <- inSampleDataList[[i]]
#  currentATR <- weeklyATRs[[i]]
#  currentVIX <- weeklyVIXs[[i]]
#  standardisedATRs <- zScoreStandardisation(calculateATRForRangeXTS(series,7))
#  standardisedVIXs <- zScoreStandardisation(calculateVIXForRangeXTS(series, 7))
#  combinedATRVIX <- stdATRandVIX(standardisedATRs, standardisedVIXs)
# Print any additional information you need
#  volatiltiyAnalysis <- analyseMonthlyVol(series, 7)
#  monthlyVolatility[[length(monthlyVolatility) + 1]] <- volatiltiyAnalysis
#}
#print(monthlyVolatility)

