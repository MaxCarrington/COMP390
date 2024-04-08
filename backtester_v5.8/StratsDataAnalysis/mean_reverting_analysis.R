#-------------------------------------------------------------------------------
#  Analyses time series for mean reverting traits. Each trait is given a score
#  which is out of 100
#-------------------------------------------------------------------------------


source('./Indicators/ADF_test.R')
source('./Indicators/hurst_exponent.R')
source('./Indicators/half_life_of_mean_reversion.R')
source('./Indicators/variance_ratio_test.R')

#-------------------------------------------------------------------------------
#  getSeriesMeanRevStats
#   - Calculate the variance ratio for each time series
#   - Calculate the half life of mean-reversion
#   - Calculate the ADF test
#   - Calculate the Hurst exponent
#   - Score each time series out of 100 based on above
#   - If score is higher than the set amount, the time series will be used for 
#     mean reversion 
#-------------------------------------------------------------------------------
getSeriesMeanRevStats <- function(series, index, threshold){
  
  score <- 0
  consistencyScore <- 0
  
  HVal <- calculateHurstExponent(series$Close)
  RndWalk <- performVarianceRatioTest(series$Close)
  UnitRoot <- performADFTest(series$Close)
  HalfLife <- calculateHalfLife(series$Close) 
  
  if(HVal < 0.5){
    score <- score + 20
    consistencyScore <- consistencyScore + 1
  }
  else if(HVal > 0.45 && HVal < 0.55){
    score <- score + 5
  }
  if (length(RndWalk) > 0){
    score <- score + 15
    consistencyScore <- consistencyScore + 1
  }
  if(!is.na(UnitRoot$p.value) && UnitRoot$p.value < threshold){
    score <- score + 20
    consistencyScore <- consistencyScore + 1
  }
  meanRevAttributes <- list(HVal = HVal,
                            RndWalk = RndWalk,
                            UnitRoot = UnitRoot,
                            HalfLife = HalfLife
  )       
  halfLifeDuration <- determineHalfLife(meanRevAttributes)
  halfLifeScore <- calcHalfLifeScore(halfLifeDuration)
  score <- score + halfLifeScore
  
  if(score > 0 && consistencyScore == 3)
    score <- score + 20
  
  meanRevertingInfo <- list(attributes = meanRevAttributes,
                            meanRevScore = score,
                            halfLifeDuration = halfLifeDuration,
                            seriesIndex = index
                            )
  return(meanRevertingInfo)
}

# based on a half life, gives the score to be added to the total score
calcHalfLifeScore <- function(halfLife){
  score <- switch(halfLife,
         "SHORT" = 25,
         "MEDIUM" = 15,
         "LONG" = 5,
         "TOO_LONG" = 0)
  return(score)
}

#Determines the size of the half life of a time series based on the information of the time series atrributes
determineHalfLife <- function(attributes){
  duration <- ""
  if(attributes$HalfLife$HalfLife_WithIntercept < 15 || attributes$HalfLife$HalfLife_NoIntercept < 15){
    duration <- "SHORT"
  }
  else if(attributes$HalfLife$HalfLife_WithIntercept < 32 || attributes$HalfLife$HalfLife_NoIntercept < 32){
    duration <- "MEDIUM"
  }
  else if(attributes$HalfLife$HalfLife_WithIntercept < 52 || attributes$HalfLife$HalfLife_NoIntercept < 52){
    duration <- "LONG"
  }
  else{
    duration <- "TOO_LONG"
  }
  return(duration)
}



