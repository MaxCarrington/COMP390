#-------------------------------------------------------------------------------
# - Volatility Analysis
#   - Calculate the ATR and the VIX
#   - Normalise the indicators, using the Z-score
#   - Create a combined volatility score based on the normalised values
#   - Define volatility thresholds
#   - Analyse the combined score over time to identify perios of high/low volatility
#   - correlation Analysis: Explore the correlation between ATR and VIX
#-------------------------------------------------------------------------------
source('./Indicators/average_true_range.R')
source('./Indicators/volatility_index.R')

analyseMonthlyVolatility <- function(series, lookbackSizes){
  #Calculate the fortnightly indicator and weeklyVIXs
  weeksToCheck = (length(inSampleDataList[[1]]$Close) / 7)
  weeklyATRs<- calculateATRForRange(series, numToCheck = weeksToCheck, lookback = lookbackSizes$weekly, rangeIncrease = 7)
  weeklyVIXs <- calculateVIXForRange(series, numToCheck = weeksToCheck, lookback = lookbackSizes$weekly, rangeIncrease = 7)
  
  normalisedATRs <- zScoreNormalisation(weeklyATRs)
  normalisedVIXs <- zScoreNormalisation(weeklyVIXs)
  
  combinedATRVIX <- normATRandVIX(weeklyATRs, normalisedVIXs)
  
  volatilityClassifications <- classifyVolatility(combinedATRVIX)
  
  numberOfMonths <- floor(length(inSampleDataList[[1]]$Close) / 7) /4
  endOfMonthIndex <- 4
  startOfMonthIndex <- 1
  
  overallVolatility <- c()
  for(i in 1:numberOfMonths){
    weeks <- determineSeriesVolatility(volatilityClassifications[startOfMonthIndex:endOfMonthIndex])
    overallVolatility <- c(overallVolatility, weeks)
    startOfMonthIndex <- endOfMonthIndex + 1
    endOfMonthIndex <- endOfMonthIndex + 4
  }
  return(overallVolatility)
}

analyseSereiesVolatility <- function(monthlyVolatility){
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
  print(frequencyTable)
  
  if(numNonVolatile >= totalScores*0.70){
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
#Calculates combined ATR and VIX volatility threshold values based on the current time series to determine times of high, low, medium volatility
combinedVolatilityThreshs <- function(combinedATRVIX){
  volatilityPeriods <- c()
  avgVol <- mean(combinedATRVIX)
  sdVol <- sd(combinedATRVIX)
  
  for(i in seq_along(combinedATRVIX)){
    if(combinedATRVIX[i] < avgVol + sdVol * 0.5){
      volatilityPeriods <- c(volatilityPeriods, "Low")
    } else if(combinedATRVIX[i] >= avgVol + sdVol * 0.5 && combinedATRVIX[i] < avgVol + sdVol * 1.5){
      volatilityPeriods <- c(volatilityPeriods, "Medium")
    } else if(combinedATRVIX[i] >= avgVol + sdVol * 1.5 && combinedATRVIX[i] < avgVol + sdVol * 2){
      volatilityPeriods <- c(volatilityPeriods, "High")
    } else {
      volatilityPeriods <- c(volatilityPeriods, "Very High")
    }
  }
  return(volatilityPeriods)
}





#Compute mean and standard deviation for each list, and apply the zScore forumla 
zScoreNormalisation <- function(indicatorValues){
  mean <- mean(indicatorValues, na.rm = TRUE)
  stdDev <- stats::sd(indicatorValues, na.rm = TRUE)
  zScore <- (indicatorValues - mean) /stdDev
  return(zScore)
}

#Normalise the volatilitis base on the z scores
normATRandVIX <- function(normindicator, normVIXs) {
  combinedScores <- list()
  
  #average and combine both atr and vix
  combinedScores <- c()
  for (i in seq_along(normindicator)) {
    combinedScore <- (normindicator[[i]] + normVIXs[[i]]) / 2
    combinedScores <- c(combinedScores, combinedScore)
  }
  normalisedScores <- zScoreNormalisation(combinedScores)
  return(normalisedScores)
}
