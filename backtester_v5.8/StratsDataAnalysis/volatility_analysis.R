#-------------------------------------------------------------------------------
# - Volatility Analysis
#   - Calculate the ATR, VIX and std of log returns  
#   - Standardise the ATR and VIX indicators, using the Z-score
#   - Create a combined volatility score based on the normalised values
#   - Define volatility thresholds
#   - Analyse the combined score over time to identify perios of high/low volatility
#   - correlation Analysis: Explore the correlation between ATR and VIX
#-------------------------------------------------------------------------------
source('./Indicators/average_true_range.R')
source('./Indicators/volatility_index.R')
source('./Plot/plot_volatility.R')

analysePeriodVol <- function(series, lookback, monthly=FALSE){
  stdDev <- stdDevRollingWindow(series, lookback)
  #Standardises average true range, volatility index and standard deviation.
  standardisedATRs <- zScoreStandardisation(calculateRollingATR(series, lookback))
  standardisedVIXs <- zScoreStandardisation(calculateRollingVIX(series, lookback))
  standardisedStdDev <- zScoreStandardisation(stdDevRollingWindow(series, lookback))
  #Combines into one indicator
  combinedATRVIXStdDev <- stdATRVIXStdDev(standardisedATRs, standardisedVIXs, standardisedStdDev)
  volClassifications <- classifyVolatility(combinedATRVIXStdDev)
  #Calculates an overall volatility and period volatility based on a set number of periods
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
  return(list(periodVol = volClassifications, seriesVol = seriesVol))
}

analyseSeriesVol <- function(monthlyVolatility){
  # Define category labels
  nonVolatile = "Non-Volatile"
  volatile = "Volatile"
  highlyVolatile = "Highly Volatile"
  
  frequencyTable <- table(monthlyVolatility)
  
  # Correctly calculate the total number of periods
  totalScores <- sum(frequencyTable)
  
  # Access frequency counts, ensuring to handle cases where a category might be missing
  numNonVolatile <- ifelse("Low" %in% names(frequencyTable), frequencyTable["Low"], 0)
  numVolatile <- ifelse("Medium" %in% names(frequencyTable), frequencyTable["Medium"], 0) +
    ifelse("High" %in% names(frequencyTable), frequencyTable["High"], 0)
  numHighVolatile <- ifelse("Highly Volatile" %in% names(frequencyTable), frequencyTable["Highly Volatile"], 0)
  
  # Classification logic based on the distribution of volatility categories
  if(numNonVolatile >= totalScores*0.70){
    return(nonVolatile)
  } else if(numHighVolatile/totalScores > 0.10){
    return(highlyVolatile)
  } else {
    return(volatile)
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
  volatilityScores <- na.omit(volatilityScores)
  meanScore <- mean(volatilityScores, na.rm = TRUE)
  sdScore <- sd(volatilityScores, na.rm = TRUE)
  
  highVolPercentileThreshold <- quantile(volatilityScores, 0.7)  # Top 30%
  highPercentileThreshold <- quantile(volatilityScores, 0.6)     # Top 40%
  
  highVolSDThreshold <- meanScore + 1.5 * sdScore
  highSDThreshold <- meanScore + sdScore           
  
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
#Combine the standardised std dev, atr and vix and divide by 3 to give a score
stdATRVIXStdDev <- function(standardisedATRs, standardisedVIXs, standardisedStdDev) {
  combinedScores <- (standardisedATRs + standardisedVIXs + standardisedStdDev) / 3
  
  return(combinedScores)
}



# Calculate the standard deviation of returns over a rolling window
stdDevRollingWindow <- function(series, lookback){
  print(series$Close)
  print(lookback)
  returns <- diff(log(ifelse(series$Close > 0, series$Close, NA)), lag = 1)
  print(returns)
  rollingStdDev <- runSD(returns, n = lookback)
  print(rollingStdDev)
  return(rollingStdDev)
}

