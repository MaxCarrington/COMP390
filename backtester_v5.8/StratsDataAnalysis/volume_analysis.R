source('./Plot/vector_plot.R')
# Determines if volume spikes are correlated with mean reversion
volMRCorrelation <- function(series, windowSize, threshold) {
  corScoreValue <- 1
  noCorScoreValue <- 0
  
  # Calculate EMA of trading volume
  emaVolume <- na.omit(TTR::EMA(series$Volume, n=windowSize))
  # Determine threshold for volume spikes based on EMA
  volThresh <- quantile(emaVolume, threshold)
  
  # Identify periods of volume spikes
  volSpikes <- series$Volume > volThresh
  volSpikePeriods <- which(volSpikes)
  scores <- numeric(length(volSpikePeriods))
  
  for (i in seq_along(volSpikePeriods)) {
    startPeriod <- volSpikePeriods[i]
    if(startPeriod != 1){
      
      meanRevertPrice <- calculatePreviousMeanEMA(series$Close, windowSize, startPeriod)
      endPeriod <- min(startPeriod + windowSize, length(series$Close))
      lastPrice <- last(series$Close[startPeriod:endPeriod])
      
      scores[i] <- scoreSpike(lastPrice, meanRevertPrice, threshold, corScoreValue, noCorScoreValue)
    }
  }
  rankingScore <- mean(scores)
  return(rankingScore)
}
scoreSpike <- function(lastPrice, meanRevertPrice, threshold, corScoreValue, noCorScoreValue) {
  lastPrice <- coredata(lastPrice)
  meanRevertPrice <- coredata(meanRevertPrice)
  if (abs(lastPrice - meanRevertPrice) < threshold) {
    return(corScoreValue)  # Correlated
  } else {
    return(noCorScoreValue)  # Not correlated
  }
}

calculatePreviousMeanEMA <- function(series, windowSize, startPeriodDate) {
  # Extract the series up to the start period date
  relevantSeries <- series[index(series) < startPeriodDate]
  
  if(length(relevantSeries) >= windowSize) {
    emaValues <- TTR::EMA(relevantSeries, n=windowSize)
    meanRevertPrice <- tail(na.omit(emaValues), 1)  # Use the last non-NA EMA value before the startPeriodDate
  } else {
    meanRevertPrice <- mean(relevantSeries, na.rm = TRUE)  # Fallback if not enough data for EMA
  }
  
  return(meanRevertPrice)
}



# For momentum, analyze increasing volume trends alongside price increases.
volIncWithTrend <- function(series, windowSize){
  # Exponentially Weighted Moving Average of the trading volume
  ewmaVol <- TTR::EMA(series$Volume, windowSize)
  
  # Detect price increases and calculate EWMA for price increases
  priceIncreases <- detectPriceIncrease(series$Close)
  ewmaPriceInc <- TTR::EMA(priceIncreases, windowSize)
  
  # Ensure lengths match for correlation calculation
  minLength <- min(length(ewmaVol), length(ewmaPriceInc))
  ewmaVol <- ewmaVol[(length(ewmaVol)-minLength+1):length(ewmaVol)]
  ewmaPriceInc <- ewmaPriceInc[(length(ewmaPriceInc)-minLength+1):length(ewmaPriceInc)]
  normPrices <- zScoreStandardisation(ewmaPriceInc)
  normVol <- zScoreStandardisation(ewmaVol)
  
  
  # Correlation between volume increases (as reflected by EWMA) and the EWMA of price increases
  testResult <- cor.test(normVol, normPrices, method = "pearson")
  
  # Extract correlation coefficient and p-value
  correlation <- testResult$estimate
  pValue <- testResult$p.value
  
  return(list(correlation = correlation, pValue = pValue))
}

# Adjust the detectPriceIncrease function if needed to output a numeric


detectPriceIncrease <- function(prices) {
  priceChanges <- diff(prices)
  priceIncreases <- priceChanges > 0
  return(priceIncreases)
}
#For market making, focus on identifying high liquidity periods with narrow bid-ask spreads
#or high volume periods indicating active trading.

# Assuming 'series' contains Open, High, Low, Close, Volume columns
combinedLiquidityAnalysis <- function(series, volLookback, liquidityThresh, windowSize) {
  # Step 1: Identify high volume periods
  highVolumePeriods <- highLiquidityPeriods(series, volLookback, liquidityThresh)
  # Prepare series data for liquidity estimation
  highVolSeries <- series[highVolumePeriods, ]
  # Step 2: Estimate liquidity in high volume periods
  liquidityIndicators <- estimateLiquidity(highVolSeries, windowSize)
  # Combine information from both steps
  # Assuming we're interested in periods identified as high liquidity within high volume periods
  # High liquidity and volume periods means buy and sell orders can be filled easily w/o significant price movements and the MMaker
  # can operate with tighter spreads
  finalSelection <- highVolumePeriods[!is.na(liquidityIndicators$highLiquidity == 1)]
  
  return(list(
    highVolumePeriods = highVolumePeriods,
    finalSelection = finalSelection,
    liquidityIndicators = liquidityIndicators,
    totalPeriods = length(series$Volume) - (volLookback - 1),
    lookback = volLookback,
    liquidityThresh = liquidityThresh, 
    windowSize = windowSize
  ))
}

# Function adjustments for proper execution
highLiquidityPeriods <- function(series, volLookback, threshold) {
  rollingVolume <- SMA(series$Volume, n = volLookback)
  highVolumeThreshold <- quantile(rollingVolume, probs = threshold, na.rm = TRUE)
  highVolumePeriods <- which(rollingVolume > highVolumeThreshold)
  
  return(highVolumePeriods)
}

estimateLiquidity <- function(series, windowSize = 20) {
  # Assuming 'series' has High, Low, Close, Volume columns
  windowSize <- 10
  if(windowSize > nrow(series))
    windowSize <- nrow(series)
  volatility <- (series$High - series$Low) / series$Low
  # VWAP helps to ensure you are executing trades favorably compared to the market. The VWAP data can guide the pricing of orders to 
  #improve the execution quality during these selected high volume and liquidity periods
  vwap <- (series$High + series$Low + series$Close) / 3
  avgVolume <- runMean(series$Volume, n = windowSize)
  avgVolatility <- runMean(volatility, n = windowSize)
  
  highLiquidity <- ifelse(volatility < avgVolatility & series$Volume > avgVolume, 1, 0)
  return(list(highLiquidity = highLiquidity, vwap = vwap))
}

# Note: Ensure 'series' is properly prepared and contains the necessary columns.
# Example call:
# results <- combinedLiquidityAnalysis(series)



#Compute mean and standard deviation for each list, and apply the zScore forumla 
zScoreStandardisation <- function(values){
  mean <- mean(values, na.rm = TRUE)
  stdDev <- stats::sd(values, na.rm = TRUE)
  zScore <- (values - mean) /stdDev
  
  return(zScore)
}

standardPricenVol <- function(normPrices, normVolume){
  combinedScores <- (normPrices + normVolume) / 2
  
  return(combinedScores) 
}

# Use the volume to determine if we are currently in a high liquidity periods.
isHighLiquidity <- function(volume, lookback, threshold) {
  # Calculate rolling average or median of volume for the lookback period
  rollingVolume <- rollapply(volume, width = lookback, FUN = median, align = 'right', fill = NA)
  
  # Current volume
  currentVolume <- tail(volume, n = 1)
  
  # Determine if the current volume is significantly higher than the rolling average/median
  highLiquidity <- currentVolume > (threshold * tail(rollingVolume, n = 1))
  
  return(highLiquidity)
}


#windowSizes <- c(20, 21, 22,23,25, 27, 29 ,30, 32, 34,35, 37, 38 ,40)
#thresholdPercentages <- c(0.90,0.91, 0.92, 0.93, 0.94,0.95,0.96, 0.97,0.98, 0.99)
# Split each series into training and validation segments
#trainingList <- lapply(inSampleDataList, function(series){
 # series[1:(length(series$Close)*0.8)]
#})
#validationList <- lapply(inSampleDataList, function(series){
 # series[(length(series$Close)*0.8 + 1):length(series$Close)]
#})

# Grid Search on Training Data
#optimalPairsList <- list()
#for (series in trainingList) {
 # bestScore <- -Inf
  #bestPair <- NULL
  #for (windowSize in windowSizes) {
   # for (thresholdPercentage in thresholdPercentages) {
    #  score <- volMRCorrelation(series, windowSize, thresholdPercentage)
     # if (score > bestScore) {
      #  bestScore <- score
       # bestPair <- list(windowSize = windowSize, threshold = thresholdPercentage)
      #}
    #}
  #}
  #optimalPairsList[[length(optimalPairsList) + 1]] <- bestPair
#}
#print(optimalPairsList)
