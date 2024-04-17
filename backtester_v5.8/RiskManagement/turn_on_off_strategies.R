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
checkMeanReversion <- function(series, seriesIndex, pValueThreshMR, mrScoreThresh = 60){
  meanRevStats <- analyseMR(series, seriesIndex, pValueThreshMR)
  halfLife <- round(meanRevStats$attributes$HalfLife$HalfLife_WithIntercept)
  if(meanRevStats$meanRevScore < mrScoreThresh || halfLife < 0){
    return(list(halfLife = halfLife, strategyOn = FALSE))
  } else{
    return(list(halfLife = halfLife, strategyOn = TRUE))
  }
}
#Analyses the most recent periods to determine if the market is volatile or not
recentPeriodVol <- function(periodVol){
  totalScore <- 0
  for(i in 1:nrow(periodVol)){
    totalScore <- totalScore + volScore(coredata(periodVol[i]))
  }
  if(totalScore > nrow(periodVol) * 1.75)
    return(TRUE)
  else
    return(FALSE)
}
# Function to calculate scores based on volatility categories
volScore <- function(volCategory){
  score <- switch(coredata(volCategory),
                  "Highly Volatile" = 4,
                  "High" = 3,
                  "Medium" = 1.5,
                  "Low" = 1,
                  0)
  return(score)
}
checkVolatility <- function(series, lookbacl){
  volatility <- analyseVolatility(series, lookback)
  noVolRows <- nrow(volatility$periodVol)
  isVolatile <- recentPeriodVol(volatility$periodVol[(noVolRows - (lookback - 1)):noVolRows])
  return(isVolatile)
}