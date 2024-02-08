source('./StratsDataAnalysis/mean_reverting_analysis.R')
source('./StratsDataAnalysis/volume_analysis.R')
source('./StratsDataAnalysis/volatility_analysis.R')
source('./StratsDataAnalysis/momentum_analysis.R')
source('./Indicators/average_true_range.R')
source('./Indicators/volatility_index.R')
source('./Indicators/variance_ratio_test.R')
source('./Indicators/hurst_exponent.R')

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
  logReturns <- toLogReturns(series$Close)
  isStationary <- checkStationarity(logReturns)
  
  if (!isStationary) {
    return(NULL) # The series is not stationary; handle accordingly
  }
  # Calculate rolling correlations with dates
  pastFutureReturns <- calcOptimalWindowSize(logReturns)
  momentumStratIndication <- checkLenForMomentum(lengthThresh, pastFutureReturns)
  suitableForStrat <- statisticallySuitable(momentumStratIndication, logReturns)
  if(suitableForStrat != "None"){
    return(list(stratType = suitableForStrat,
                correlations = pastFutureReturns))
  }
  else{
    return(list(stratType = "None",
                correlations = NA))
  }
 
}