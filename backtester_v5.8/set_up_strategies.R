source('./StratsDataAnalysis/master_analysis.R')
source('./example_strategies.R');
suitableStratslist <- function(inSampleDataList){
 
  strategies <- list(meanReversion = "Mean-Reversion",
                     momentum ="Momentum",
                     marketMaking = "Market-Making")
  index <- 2
  lookback <- 7
  volThresh <- 0.85
  volWSize <- 30
  pValueThreshMR <- 0.95
  momentumWSize <- 30
  pValueThreshMom <- 0.95
  
  momentumLenThresh <- 0.90
  # Initialize an empty list to hold the analysis and strategy for each series
  seriesAnalysisInfo <- list()
  # Loop through each series in the dataset
  for(i in 1:length(inSampleDataList)) {
    series <- inSampleDataList[[i]]
    
    # Analyze the series for various factors
    volatilityStats <- analyseVolatility(series, lookback)
    volumeStats <- analyseVolume(series, lookback, volWSize, volThresh)
    momentumStats <- analyseMomentum(series, momentumWSize, pValueThreshMom, momentumLenThresh)
    
    mrStats <- analyseMR(series, i, pValueThreshMR)
    if(momentumStats$stratType == strategies$meanReversion){
      mrStats$meanRevScore <- mrStats$meanRevScore + 5
    }
    # Determine the strategy based on the analysis
    strategy <- "None" # Default strategy
    if(momentumStats$stratType == strategies$momentum) {
      strategy <- strategies$momentum
    }else if(mrStats$meanRevScore >= 40){
      strategy <- strategies$meanReversion
    } else if(volatilityStats$seriesVol == "Non-Volatile") {
      strategy <- strategies$marketMaking
    }
    # Append the analysis and determined strategy to the list
    seriesAnalysisInfo[[length(seriesAnalysisInfo) + 1]] <- list(
      volatility = volatilityStats,
      volume = volumeStats,
      momentum = momentumStats,
      meanReversion = mrStats,
      index = i,
      strategy = strategy # Include the strategy directly
    )
  }
  return(seriesAnalysisInfo)
}

getMeanRevInfo <- function(seriesAnalysisInfo){
  # Extract Mean-Reversion Series Indexes
  seriesIndexes <- sapply(seriesAnalysisInfo, function(x){
    if(x$strategy == "Mean-Reversion")
      x$index
    else
      NA
  })
  seriesIndexes <- na.omit(seriesIndexes)
  attr(seriesIndexes, "na.action") <- NULL
  
  # Get the corresponding half life for each mean reverting time series
  halfLives <- sapply(seriesAnalysisInfo, function(x){
    if(x$strategy == "Mean-Reversion")
      x$meanReversion$attributes$HalfLife$HalfLife_WithIntercept
    else
      NA
  })
  halfLives <- na.omit(halfLives)
  attr(halfLives, "na.action") <- NULL
  return(list(halfLives = halfLives, seriesIndexes=seriesIndexes))
}

getMomentumInfo <- function(seriesAnalysisInfo){
  
  seriesIndexes <- sapply(seriesAnalysisInfo, function(x){
    if(x$strategy == "Momentum")
      x$index
    else
      NA
  })
  momentumLookbacks <- sapply(seriesAnalysisInfo, function(x){
    if(x$strategy == "Momentum")
      x$momentum$correlations$lookback
    else
      NA
  })
  seriesIndexes <- na.omit(seriesIndexes)
  attr(seriesIndexes, "na.action") <- NULL
  momentumLookbacks <- na.omit(momentumLookbacks)
  attr(momentumLookbacks, "na.action") <- NULL
  return(list(seriesIndexes=seriesIndexes, momentumLookbacks=momentumLookbacks))
}

getMMakingInfo <- function(seriesAnalysisInfo){
  # Extract Market-Making Series Indexes
  marketMakingSeriesIndexes <- sapply(seriesAnalysisInfo, function(x){
    if(x$strategy == "Market-Making")
      x$index
    else
      NA
  })
  marketMakingSeriesIndexes <- na.omit(marketMakingSeriesIndexes)
  attr(marketMakingSeriesIndexes, "na.action") <- NULL
  return(marketMakingSeriesIndexes)
}

setUpTradingParams <- function(tradingStrategy, strategies){
  #If the strategy is mean reversion, pass the relevant parameters to the strategy
  if(tradingStrategy == "mean_reversion") {
    
    mrInfo <- getMeanRevInfo(strategies)
    
    if(length(mrInfo$seriesIndexes) > 0){
      return(list(stdDev=2, series=mrInfo$seriesIndexes, halfLives=mrInfo$halfLives, posSizes=rep(1,10)))
    }
    else{
      cat("Mean reverison strategy can not run. No series are deemed suitable.", "\n")
    }
    
  }else if(tradingStrategy == "momentum") {#If the strategy is momentum, pass the relevant parameters to the strategy
    momentumInfo <- getMomentumInfo(strategies)
    if(length(momentumInfo$seriesIndexes) > 0){
      return (list(series = momentumInfo$seriesIndexes, lookback = momentumInfo$momentumLookbacks, holdingPeriod = c(12),rsiLookback = 30, emaLookback = 30, 
                 smaLookback = 30, wmaLookback = 30, maThreshold = 0.7, overboughtThresh = 60, oversoldThresh = 40, maType = "SMA"))
    }
    else{
      cat("Momentum strategy can not run. No series are deemed suitable.", "\n")
    }
    
  }else if(tradingStrategy == "market_making"){
    mmkingInfo <- getMMakingInfo(strategies)
    if(length(mmkingInfo) > 0){
      example_params[["market_making"]][["series"]] <- mmkingInfo
    }
    else{
      cat("Momentum strategy can not run. No series are deemed suitable.", "\n")
    }
  }
}
