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

#Determines if a price spike is aligned with mean reverting criteria
scoreSpike <- function(lastPrice, meanRevertPrice, threshold, corScoreValue, noCorScoreValue) {
  lastPrice <- coredata(lastPrice)
  meanRevertPrice <- coredata(meanRevertPrice)
  if (abs(lastPrice - meanRevertPrice) < threshold) {
    return(corScoreValue)
  } else {
    return(noCorScoreValue)
  }
}
#Calculates an EMA of prices before a given period for mean reversion reference.
calculatePreviousMeanEMA <- function(series, windowSize, startPeriodDate) {
  relevantSeries <- series[index(series) < startPeriodDate]
  
  if(length(relevantSeries) >= windowSize) {
    emaValues <- TTR::EMA(relevantSeries, n=windowSize)
    meanRevertPrice <- tail(na.omit(emaValues), 1)
  } else {
    meanRevertPrice <- mean(relevantSeries, na.rm = TRUE)
  }
  
  return(meanRevertPrice)
}

# For momentum, analyze increasing volume trends alongside price increases.
volIncWithTrend <- function(series, windowSize){
  
  ewmaVol <- TTR::EMA(series$Volume, windowSize)
  
  priceIncreases <- detectPriceIncrease(series$Close)
  ewmaPriceInc <- TTR::EMA(priceIncreases, windowSize)
  
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


#Identifies periods where prices increase compared to the previous period
detectPriceIncrease <- function(prices) {
  priceChanges <- diff(prices)
  priceIncreases <- priceChanges > 0
  return(priceIncreases)
}
#Analyses  periods of high trading volume and liquidity 
combinedLiquidityAnalysis <- function(series, volLookback, liquidityThresh, windowSize) {
  highVolumePeriods <- highLiquidityPeriods(series, volLookback, liquidityThresh)
  highVolSeries <- series[highVolumePeriods, ]
  liquidityIndicators <- estimateLiquidity(highVolSeries, windowSize)
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

# Identifies periods where trading volume is higher than a rolling average threshold
highLiquidityPeriods <- function(series, volLookback, threshold) {
  rollingVolume <- SMA(series$Volume, n = volLookback)
  highVolumeThreshold <- quantile(rollingVolume, probs = threshold, na.rm = TRUE)
  highVolumePeriods <- which(rollingVolume > highVolumeThreshold)
  
  return(highVolumePeriods)
}

#Estimates liquidity within high volume periods using volatility and VWAP
estimateLiquidity <- function(series, windowSize = 20) {
  windowSize <- 10
  if(windowSize > nrow(series))
    windowSize <- nrow(series)
  volatility <- (series$High - series$Low) / series$Low
  vwap <- (series$High + series$Low + series$Close) / 3
  avgVolume <- runMean(series$Volume, n = windowSize)
  avgVolatility <- runMean(volatility, n = windowSize)
  
  highLiquidity <- ifelse(volatility < avgVolatility & series$Volume > avgVolume, 1, 0)
  return(list(highLiquidity = highLiquidity, vwap = vwap))
}

#Compute mean and standard deviation for each list, and apply the zScore forumla 
zScoreStandardisation <- function(values){
  mean <- mean(values, na.rm = TRUE)
  stdDev <- stats::sd(values, na.rm = TRUE)
  zScore <- (values - mean) /stdDev
  
  return(zScore)
}

# Combines normalised price and volume into a score
standardPricenVol <- function(normPrices, normVolume){
  combinedScores <- (normPrices + normVolume) / 2
  
  return(combinedScores) 
}

# Use the volume to determine if we are currently in a high liquidity periods.
isHighLiquidity <- function(volume, lookback, threshold) {
  rollingVolume <- rollapply(volume, width = lookback, FUN = median, align = 'right', fill = NA)
  currentVolume <- tail(volume, n = 1)
  highLiquidity <- currentVolume > (threshold * tail(rollingVolume, n = 1))
  
  return(highLiquidity)
}
