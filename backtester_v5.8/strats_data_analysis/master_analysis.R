source('./strats_data_analysis/mean_reverting_analysis.R')
source('./strats_data_analysis/volume_analysis.R')
source('./strats_data_analysis/volatility_analysis.R')
source('./strats_data_analysis/momentum_analysis.R')
source('./Indicators/average_true_range.R')
source('./Indicators/volatility_index.R')

analyseVolatility <- function(series, lookback){
  return(analyseMonthlyVol(series, lookback))
}

analyseVolume <- function(series, lookback, windowSize, threshold){
  volMRCorrelation(series, windowSize, threshold)
  volIncWithTrend(series, windowSize)
  combinedLiquidityAnalysis(series, lookback, threshold, windowSize)
}

analyseMR <- function(series, index, threshold){
  getSeriesMeanRevStats(series, index, threshold)
}

analyseMomentum <- function(series, windowSize, pValueThresh, lengthThresh) {
  logReturns <- toLogReturns(series)
  isStationary <- checkStationarity(logReturns)
  if (!isStationary) {
    return(NULL) # The series is not stationary; handle accordingly
  }
  
  # Calculate rolling correlations with dates
  rollingCorrsWithDates <- calculateRollingCorrelationsWithDates(logReturns, windowSize)
  suitableForStrat <- checkLenForMomentum(lengthThresh, rollingCorrsWithDates)
  # Initialize an empty list to store significant correlations
  
  return(list(correlation = rollingCorrsWithDates,
              stratType = suitableForStrat))
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

