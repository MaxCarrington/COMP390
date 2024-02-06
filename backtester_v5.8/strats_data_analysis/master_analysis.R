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
  momentumStratIndication <- checkLenForMomentum(lengthThresh, rollingCorrsWithDates)
  #Calculate the hurst exponent of the series. If H < 0.5, the series is mean reverting, if H > 0.5, series is trending, if H =0.5, Random walk
  hurstExponent <- calculateHurstExponent(na.omit(toLogReturns(series$Close)))
  vratio <- performVarianceRatioTest(series$Close)
  suitableForStrat <- statisticallySuitable(momentumStratIndication, hurstExponent, vratio)
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

statisticallySuitable <- function(suitableForStrat, hurstExp, vratio){
  if (any(is.na(vratio$Stats))) {
    return("None")
  }
  else if(hurstExp < 0.5 && vratio$Stats$M2 < 1 && suitableForStrat == "Mean-Reversion"){
    return("Mean-Reversion")
  }else if(hurstExp > 0.5 && vratio$Stats$M2 > 1 && suitableForStrat == "Momentum"){
    return("Momentum")
  }
  else{
    return("None")
  }
}

