#-------------------------------------------------------------------------------
# - Mean reverting series analysis
#   - Calculate the variance ratio for each time series and if the null 
#   - Calculate the half life of mean-reversion
#   - Calculate the ADF test
#   - Calculate the Hurst exponent
#   - Score each time series out of 100 based on above
#   - If score is higher than 20 use.
#-------------------------------------------------------------------------------

#Scoring is done out of 100
source('./Indicators/ADF_test.R')
source('./Indicators/hurst_exponent.R')
source('./Indicators/half_life_of_mean_reversion.R')
source('./Indicators/variance_ratio_test.R')

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
getMeanRevSeries <- function(seriesList){
  index <- 1
  suitableSeries <- lapply(seriesList, function(series){
    seriesStats <- getSeriesMeanRevStats(series, index)
    index <- index + 1
    if (seriesStats$meanRevScore >= 20){
      return(seriesStats)
    }
  })
  filteredSeries <- Filter(Negate(is.null), suitableSeries)
  return(filteredSeries)
  }

calcHalfLifeScore <- function(halfLife){
  score <- switch(halfLife,
         "SHORT" = 25,
         "MEDIUM" = 15,
         "LONG" = 5,
         "TOO_LONG" = 0)
  return(score)
}

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

#meanRevSuitableList <- list()
#for(i in 1:10){
  #seriesMeanRevStats <- getSeriesMeanRevStats(inSampleDataList[[i]], i)
  #if(seriesMeanRevStats$meanRevScore >= 20){
    #meanRevSuitableList[[length(meanRevSuitableList) + 1]] <- seriesMeanRevStats
  #}
#}
#print(meanRevSuitableList)



