
#-------------------------------------------------------------------------------
# This file analyses the time series to determine what time series to apply a strategy to. After time series
# have been analysed, the parameters are set up for each strategy.
#-------------------------------------------------------------------------------
source('./StratsDataAnalysis/master_analysis.R')
source('./example_strategies.R');
suitableStratslist <- function(inSampleDataList){
  #List of each strategy type, used for string concatenation
  strategies <- list(meanReversion = "Mean-Reversion",
                     momentum ="Momentum",
                     marketMaking = "Market-Making")
  
  #-----------------------------------------------------------------------------
  #Parameters are set up and can be optimised here 
  liquidityWindowSize <- 30
  lookback <- 100
  pValueThreshMR <- 0.95
  momentumWSize <- 30
  pValueThreshMom <- 0.95
  volatilityLookback <- 20
  monthly <- FALSE
  volumeLookback <- 20
  liquidityThresh <- 0.95
  periodThresh <- 0.95
  momentumLenThresh <- 0.90
  priceLookback <- 15
  volumeMultiplier <- 1.25 
  rangeMultiplier <- 0.75
  
  #-----------------------------------------------------------------------------
  
  # Initialise an empty list to hold the analysis and strategy for each series
  seriesAnalysisInfo <- list()
  # Loop through each series in the dataset
  for(i in 1:length(inSampleDataList)) {
    series <- inSampleDataList[[i]]
    
    # Analyse the series for volatility, liquidity, momentum statistics, mean reversion statistics
    volatilityStats <- analyseVolatility(series, volatilityLookback)
    liquidityStats <- analyseLiquidity(series, volumeLookback, liquidityThresh, windowSize, priceLookback, volumeMultiplier, rangeMultiplier)
    momentumStats <- analyseMomentum(series, momentumWSize, pValueThreshMom, momentumLenThresh)
    mrStats <- analyseMR(series, i, pValueThreshMR)
    numOnes <- sum(liquidityStats$liquidityIndicators$highLiquidity == 1, na.rm = TRUE)
    
    # Update the mean reversion score if the strategy 
    if(momentumStats$stratType == strategies$meanReversion){
      mrStats$meanRevScore <- mrStats$meanRevScore + 5
    }
    
    # Determine the strategy based on the analysis above
    strategy <- "None" # Default strategy
    if(momentumStats$stratType == strategies$momentum) {
      strategy <- strategies$momentum
    }else if(mrStats$meanRevScore >= 40){
      strategy <- strategies$meanReversion
    } else if(volatilityStats$seriesVol == "Non-Volatile") {
      #Count the number of periods of high liquidity
      numOnes <- sum(liquidityStats$liquidityIndicators$highLiquidity == 1, na.rm = TRUE)
      if(numOnes > (length(liquidityStats$liquidityIndicators$highLiquidity)*0.18))
        strategy <- strategies$marketMaking 
    }
    
    print(paste("Strategy for series", i, "is", strategy))
    
    # Append the analysis and determined strategy to the list
    seriesAnalysisInfo[[length(seriesAnalysisInfo) + 1]] <- list(
      volatility = volatilityStats,
      volumeStats = liquidityStats, #Change this to be different for all
      momentum = momentumStats,
      meanReversion = mrStats,
      index = i,
      strategy = strategy # Include the strategy directly
    )
  }
  return(seriesAnalysisInfo)
}
#-------------------------------------------------------------------------------
# All of the functions below extract parameters for strategies to use based on 
# the data analysis of time series.

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
    if(x$strategy == "Mean-Reversion"){
      round(x$meanReversion$attributes$HalfLife$HalfLife_WithIntercept)
    }
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
    if(x$strategy == "Momentum"){
      x$momentum$correlations$lookback
    }
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
    if(x$strategy == "Market-Making"){
      x$index
    }
    else
      NA
  })
  marketMakingSeriesIndexes <- na.omit(marketMakingSeriesIndexes)
  attr(marketMakingSeriesIndexes, "na.action") <- NULL
  marketMakingVolLookback <- sapply(seriesAnalysisInfo, function(x){
    if(x$strategy == "Market-Making"){
      x$volumeStats$lookback
    }
    else
      NA
  })
  marketMakingVolLookback <- na.omit(marketMakingVolLookback)
  attr(marketMakingVolLookback, "na.action") <- NULL
  
  marketMakingliquidityThresh<- sapply(seriesAnalysisInfo, function(x){
    if(x$strategy == "Market-Making"){
      x$volumeStats$liquidityThresh
    }
    else
      NA
  })
  marketMakingliquidityThresh <- na.omit(marketMakingliquidityThresh)
  attr(marketMakingliquidityThresh, "na.action") <- NULL
  
  marketMakingWindowSize<- sapply(seriesAnalysisInfo, function(x){
    if(x$strategy == "Market-Making"){
      x$volumeStats$windowSize
    }
    else
      NA
  })
  marketMakingWindowSize <- na.omit(marketMakingWindowSize)
  attr(marketMakingWindowSize, "na.action") <- NULL
  
  return(list(marketMakingVolLookback=marketMakingVolLookback, 
              marketMakingSeriesIndexes=marketMakingSeriesIndexes, 
              marketMakingliquidityThresh = marketMakingliquidityThresh,  
              marketMakingWindowSize = marketMakingWindowSize))
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# setUpTradingParams is used to set up ALL of the parameters for stragies to use,
# these being both parameters from the data analysis above as well as parameters 
# that are "hard coded" in (that have been optimised correctly)
setUpTradingParams <- function(tradingStrategy, strategies){
  #If the strategy is mean reversion, pass the relevant parameters to the strategy
  if(tradingStrategy == "mean_reversion") {
    
    mrInfo <- getMeanRevInfo(strategies)
    
    if(length(mrInfo$seriesIndexes) > 0){
      return(list(stdDev=2, 
                  series=mrInfo$seriesIndexes, 
                  halfLives=mrInfo$halfLives, 
                  posSizes=rep(1,10)))
    }
    else{
      cat("Mean reverison strategy can not run. No series are deemed suitable.", "\n")
    }
    
  }else if(tradingStrategy == "momentum") {#If the strategy is momentum, pass the relevant parameters to the strategy
    momentumInfo <- getMomentumInfo(strategies)
    if(length(momentumInfo$seriesIndexes) > 0){
      return (list(series = momentumInfo$seriesIndexes, 
                   lookback = momentumInfo$momentumLookbacks, 
                   holdingPeriod = c(12),
                   rsiLookback = 30, 
                   emaLookback = 30, 
                   smaLookback = 30,
                   wmaLookback = 30, 
                   maThreshold = 0.7, 
                   overboughtThresh = 60, 
                   oversoldThresh = 40, 
                   maType = "SMA"
                   ))
    }
    else{
      cat("Momentum strategy can not run. No series are deemed suitable.", "\n")
    }
    
  }else if(tradingStrategy == "market_making"){
    mmkingInfo <- getMMakingInfo(strategies)
    if(length(mmkingInfo) > 0){
      return (list(series = mmkingInfo$marketMakingSeriesIndexes, 
                   lookback = mmkingInfo$marketMakingVolLookback, 
                   liquidityThresh =mmkingInfo$marketMakingliquidityThresh, 
                   windowSize = mmkingInfo$marketMakingWindowSize, 
                   highLiquidityPrdsThresh = 10, 
                   volatilityLookback = 10,
                   volumeLookback = 10,
                   tradeHistory = list(wins = numeric(), losses = numeric()),
                   initialConfidence = 0.5,
                   liquidityLookback = 15,
                   priceLookback = 10,
                   volumeMutliplier = 1.25, 
                   rangeMultiplier = 0.75
                   ))
    }
    else{
      cat("Market making strategy can not run. No series are deemed suitable.", "\n")
    }
  }
}
#-------------------------------------------------------------------------------
