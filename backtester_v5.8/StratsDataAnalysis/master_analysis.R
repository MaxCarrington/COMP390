#-------------------------------------------------------------------------------
# Master analysis file is used to analyse time series based on the files sourced.
# the file is used as a central point to simplify the analysis instead of having
# to call multiple files and functions
#-------------------------------------------------------------------------------
source('./StratsDataAnalysis/mean_reverting_analysis.R')
source('./StratsDataAnalysis/volume_analysis.R')
source('./StratsDataAnalysis/volatility_analysis.R')
source('./StratsDataAnalysis/momentum_analysis.R')
source('./Indicators/average_true_range.R')
source('./Indicators/volatility_index.R')
source('./Indicators/variance_ratio_test.R')
source('./Indicators/hurst_exponent.R')

#Returns volatility information of a given time series
analyseVolatility <- function(series, lookback){
  return(analysePeriodVol(series, lookback))
}
#Returns mean reversion information of a given time series
analyseMR <- function(series, index, threshold){
  getSeriesMeanRevStats(series, index, threshold)
}
#Returns momentum information of a given time series
analyseMomentum <- function(series, windowSize, pValueThresh, lengthThresh) {
  logReturns <- toLogReturns(series$Close)
  isStationary <- checkStationarity(logReturns)
  
  if (!isStationary) {
    return(NULL)
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
#Returns liquidity information of a given time series
analyseLiquidity <- function(series, volatilityLookback, monthly=FALSE, windowSize, volumeLookback, liquidityThresh, periodThresh){
  return(combinedLiquidityAnalysis(series, volumeLookback, liquidityThresh, windowSize))
}