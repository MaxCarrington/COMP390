source('./Indicators/average_true_range.R')
source('./Indicators/volatility_index.R')
source('./Indicators/half_life_of_mean_reversion.R')
#-------------------------------------------------------------------------------
# Mean Reverting Stop Losses
#-------------------------------------------------------------------------------

# Multiple of half-life as Stop Loss, use a multiplier (typically 2-3 times its half life) with the half life, If the time exceeds the half life, then
# the stop loss will be triggered
halfLifeStopL <- function(multiplier, halfLife, daysInTrade){
  if (halfLife*multiplier > daysInTrade)
    stopLossTriggered <- TRUE
  else
    stopLossTriggered <- FALSE
  return (stopLossTriggered)
    
}

# Regime change indicator. If there is a significant change in the market, the 
# mean-reverting behavior may have ended, therefore prompt stop loss
# OPTIMISE THIS, TO SIGNIFY A CHANGE IN MARKET CONDITIONS
# - How are the thresholds defined?
marketShiftStopL <- function(series, currentDay, lookback, atrThresh, vixThresh, volThresh){
  stopLossTriggered <- FALSE
  atrLookback <- calculateTrueRange(series, lookback)
  vixLookback <- annualisation(series, lookback)
  volumeLookback <- checkVolumeChange(series, currentDay, lookback, volThresh)
  if(atrLookback > atrThresh || vixLookback > vixThresh || volumeLookback > volThresh)
    stopLossTriggered <- TRUE
  return (stopLossTriggered)
  
}

# Checks the change in volume. If there is a significant change in volume between 
# the current day and the average volume of the lookback period, the change
# will be returned. If there is no significant change, then 0 is returned.
# HOW IS THE THRESHOLD DETERMINED? ADD IN THRESHOLD FUNCTIONALITY
checkVolumeChange <- function(series, currentDay, lookback, threshold){
  currentVol <- series$volume[currentDay]
  avgLookbackVol <- average(series$volume[(currentDay - lookback):currentDay])
  if(currentVol > avgLookbackVol){
    return(currentVol - avgLookbackVol)
  }else if(currentVol < avgLookbackVol){
    return(currentVol - avgLookbackVol)
  }
  else{
    return(0)
  }
}
#-------------------------------------------------------------------------------
# Market Making Stop Losses
#-------------------------------------------------------------------------------

# Inventory based stop loss - If inventory gets too high, trigger stop loss 
# (should result in a market order being placed)
inventoryStopL <- function(threshold, positions){
  if(length(positions) > threshold){
    stopLossTriggered <- TRUE
  } 
  else
    stopLossTriggered <- FALSE
  return(stopLossTriggered)
}

# Volatility Adjusted stop loss - Using ATR/VIX to set a responsive stop loss 
# based on the market conditions
#FIX THIS TO USE WHAT WE HAVE USED
volatilityStopL <- function(series, lookback, previousVolatility, threshold){
  atr <- calculateTrueRange(series, lookback)
  vix <- annualisation(series, lookback)
  currentVolatility <- (atr+vix)/2
  if (currentVolatility > (previousVolatility + threshold))
    stopLossTriggered <- TRUE
  else
    stopLossTriggered <- FALSE
  
  return(stopLossTriggered)
}

# Time based stop loss - If the time exceeds a threshold then exit position
timeBasedStopL <- function(tradeTime, threshold){
  #Or should we use the maximum holding period?
  if(tradeTime > threshold){
    stopLossTriggered <- TRUE
  }
  else{
    stopLossTriggered <- FALSE
  }
}

#-------------------------------------------------------------------------------
# Momentum Stop Losses - Improve
#-------------------------------------------------------------------------------
# Trailing Stop loss - Secures profits while allowing momentum to continue

trailingStopL <- function(series, entryPrice, trailPercent){
  stopLossTriggered <- FALSE
  for (i in 1:length(series)) {
    if (series$High[i] > entryPrice) {
      # Update the stop loss when the todays high > entry price
      stopLossPrice <- max(entryPrice, currentHigh[i] * (1 - trailPercent))
    }
    
    if (currentClose[i] <= stopLossPrice) {
      # If today's current close is less than or equal to the stop loss price, trigger the stop loss
      stopLossTriggered <- TRUE
    }
  }
  return(stopLossTriggered)
}
#-------------------------------------------------------------------------------
# Momentum Stop Losses - Improve
#-------------------------------------------------------------------------------

standardStopL <- function(stopLossPrice, currentTradePrice, direction){
  
  stopLossTriggered <- FALSE
  if((currentTradePrice <= stopLossPrice) && (direction > 0)){ #Long position
    stopLossTriggered <- TRUE
  }else if((currentTradePrice >= stopLossPrice) && (direction < 0)){ #Short position
    stopLossTriggered <- TRUE
  }
  return (stopLossTriggered)
}

#-------------------------------------------------------------------------------
# Maximum daily loss
# The maximum daily loss is the maximum allowable loss a trade can make in a day 
#-------------------------------------------------------------------------------
# currentBalance should look something like this: pfolioPnL$pfoliosPnL$CumPnL[nrow(pfolioPnL$pfoliosPnL)]
maxDailyLossOfTrade <- function(currentPrice, entryPrice, currentBalance, currentPositions, direction) {
  stopLossTriggered <- FALSE
  maxPortfolioLoss <- 0.05 * currentBalance # 5% is the maximum allowable loss for the entire Portfolio
  allowedLossPerTrade <- maxPortfolioLoss / length(currentPositions)
  
  if (direction > 0) { # Long position
    currentLoss <- entryPrice - currentPrice
    if (currentLoss > allowedLossPerTrade) {
      stopLossTriggered <- TRUE
    }
  } else if (direction < 0) { # Short position
    currentLoss <- currentPrice - entryPrice
    if (currentLoss > allowedLossPerTrade) {
      stopLossTriggered <- TRUE
    }
  }
  
  return(stopLossTriggered)
}
#-------------------------------------------------------------------------------
#Basic stop losses
#-------------------------------------------------------------------------------

#Set to half of the take profit so far
calculateStopLoss <- function(tradeType, entryPrice){
  if(tradeType == "buy"){
    stopLoss <- entryPrice * 0.875
  }else{ #Must be a sell
    stopLoss <- entryPrice * 1.125
  }
  return(coredata(stopLoss))
}
#This function checks if stop losses have been hit, if they have, add them to a 
# position so the trade is cancelled
checkStopLossesHit <- function(store, todaysOpen, seriesIndex){
  positionSize <- 0
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  for(i in 1:length(tradeRecords)){
    tradeRecord <- tradeRecords[[i]]
    tradeEntryPrice <- tradeRecord$entryPrice
    stopLoss <- tradeRecord$stopLoss
    orderType <- tradeRecord$tradeType
    isClosed <- tradeRecord$closed
    if(orderType == "buy" && todaysOpen <= stopLoss && !isClosed){
      positionSize <- positionSize -tradeRecord$positionSize
      #print(paste("A Stop loss has been hit, at price", todaysOpen, " and selling", positionSize, "units, to cancel long trade"))
      
    } else if(orderType == "sell" && todaysOpen >= stopLoss && !isClosed){
      positionSize <- positionSize + tradeRecord$positionSize
      #print(paste("A Stop loss has been hit, at price", todaysOpen, " and buying", positionSize, "units, to cancel long trade"))
    }
    if(positionSize != 0)
      store <- closeTradeRecord(store, seriesIndex, tradeRecord)
  }
  return(list(positionSize = positionSize, store = store))
}


