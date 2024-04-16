source('./StratsDataAnalysis/master_analysis.R')
strategies <- list(meanReversion = "Mean-Reversion",
                   momentum ="Momentum",
                   marketMaking = "Market-Making")

#Analyses the current series in the store to determine whether we should turn the 
#strategy on or off based on the current data for a momentum strategy
checkMomentum <- function(series, momentumWSize, pValueThreshMom, momentumLenThresh){
  momentumStats <- analyseMomentum(series, momentumWSize, pValueThreshMom, momentumLenThresh)
  if(momentumStats$stratType == strategies$momentum)
    return(TRUE)
  else
    return(FALSE)
}
#Analyses the current series in the store to determine whether we should turn the 
#strategy on or off based on the current data for a mean reversion strategy
checkMeanReversion <- function(series, seriesIndex, pValueThreshMR, mrScoreThresh = 40){
  meanRevStats <- analyseMR(series, seriesIndex, pValueThreshMR)
  halfLife <- round(meanRevStats$attributes$HalfLife$HalfLife_WithIntercept)
  if(meanRevStats$meanRevScore >= mrScoreThresh){
    return(list(halfLife = halfLife, strategyOn = TRUE))
  } else{
    return(list(halfLife = halfLife, strategyOn = FALSE))
  }
}