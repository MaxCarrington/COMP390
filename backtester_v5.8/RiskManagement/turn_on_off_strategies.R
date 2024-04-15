source('./StratsDataAnalysis/master_analysis.R')
strategies <- list(meanReversion = "Mean-Reversion",
                   momentum ="Momentum",
                   marketMaking = "Market-Making")

checkMeanReversion <- function(series, seriesIndex, pValueThreshMR, mrScoreThresh){
  mrStats <- analyseMR(series, i, pValueThreshMR)
  return(mrStats$meanRevScore >= mrScoreThresh)
}
checkMomentum <- function(series, momentumWSize, pValueThreshMom, momentumLenThresh){
  momentumStats <- analyseMomentum(series, momentumWSize, pValueThreshMom, momentumLenThresh)
  if(momentumStats$stratType == strategies$momentum)
    return(TRUE)
  else{
    return(FALSE)
  }
}
checkMarketMaking <- function(store, params){
}