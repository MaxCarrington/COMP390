source('./strats_data_analysis/mean_reverting_analysis.R')
source('./strats_data_analysis/volume_analysis.R')
source('./strats_data_analysis/volatility_analysis.R')
source('./strats_data_analysis/momentum_analysis.R')
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
  logReturns <- toLogReturns(series)
  isStationary <- checkStationarity(logReturns)
  if (!isStationary) {
    return(NULL) # The series is not stationary; handle accordingly
  }
  
  # Calculate rolling correlations with dates
  rollingCorrsWithDates <- calculateRollingCorrelationsWithDates(logReturns, windowSize)
  if(length(rollingCorrsWithDates$correlation) > 0){
    momentumStratIndication <- checkLenForMomentum(lengthThresh, rollingCorrsWithDates)
    #Calculate the hurst exponent of the series. If H < 0.5, the series is mean reverting, if H > 0.5, series is trending, if H =0.5, Random walk
    seriesMomentum <- findLookbackHoldingPair(rollingCorrsWithDates)
    hurstExponent <- calculateHurstExponent(na.omit(toLogReturns(series$Close)))
    vratio <- performVarianceRatioTest(series$Close)
    suitableForStrat <- statisticallySuitable(momentumStratIndication, hurstExponent, vratio)
    return(list(correlation = rollingCorrsWithDates,
                stratType = suitableForStrat,
                seriesMomentum = seriesMomentum))
  }
  else{
    return(list(rollingCorrsWithDates = NA,
    stratType = "None",
    seriesMomentum = NA))
  }
 
}

