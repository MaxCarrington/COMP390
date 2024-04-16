#-------------------------------------------------------------------------------
#The momentum strategy here is a very simple one. If a time series is deemed to be upwardly trending, with a trending moving average and an RSI of 
#over 70 (although this value may change after parameter optimisation), this indicates strong market momentum, buy the asset if the open is higher 
#than the previous close. Conversely, If the market is downwardly trending, with a downward trending moving average and an RSI under 30 (again, this 
#value is subject to change), short an asset when the open is lower than the close. The premise of this idea is to capitalise on the momentum created 
#by price gaps. 
#-------------------------------------------------------------------------------
source('./RiskManagement/position_size_calc.R')
source('./RiskManagement/trade_record_management.R')
source('./RiskManagement/turn_on_off_strategies.R')
getOrders <- function(store, newRowList, currentPos, info, params) {
  limitOrders <- TRUE
  positionSize <- 1
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  tradingDaysInMonth <- 21
  #Initialise the store if it is not already initialised
  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  #Update the store 
  store <- updateStore(store, newRowList, params$series)    
  #marketOrders <- -currentPos;
  pos <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  if(store$iter == 558){
    print("Wins:")
    print(length(store$tradeHistory$wins))
    print("Losses")
    print(length(store$tradeHistory$losses))
    print("Number of trades: ")
    print(store$tradeCount)
    print(info)
  }
  for (i in 1:length(params$series)) {
    #Initialise values from the store, parameters and price information
    lookback <- params$lookback[i]
    rsiLookback <- params$rsiLookback
    seriesIndex <- params$series[i]
    series <- head(store$ohlcv[[seriesIndex]], -1)
    holdingPeriod <- params$holdingPeriod
    ydaysClose <- coredata(series$Close[length(series$Close)])
    todaysOpen <- coredata(newRowList[[seriesIndex]]$Open)
    limitPrice <- 0
    #Only check if the strategy should be turned off, every 2 trading months don't want to check too often too much.
    strategyOn <- TRUE
    
    if(store$iter %% lookback == 0){
      strategyOn <- checkMomentum(series, params$momentumWSize, params$pValueThreshMom, params$momentumLenThresh)
      store$strategyOn[i] <- strategyOn
    }
    
    #Check if there are any trade records
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      if(!limitOrders){
        store <- updateEntryPrices(store, newRowList, params$series, seriesIndex)
        }#Update the entry prices in trade records as we can only use open n + 1 for record n
      
      else{ #Check if limit price has been hit
        store <- checkIfLimitPriceHit(store, newRowList, params$series, seriesIndex)
      }
        
    }
    #Initialise position adjustment factor
    adjustedPositions <- 0
    #If there are any trade records 
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      #Increment all open positions by 1
      store <- incrementHoldingPeriods(store, seriesIndex)
      #Add todays open price to all closed orders from yesterday
      store <- addExitPrice(store, seriesIndex, newRowList)
      #Handle closing of orders, stop losses and take profits 
      adjust <- adjustPositions(store, seriesIndex, holdingPeriod, positionSize, todaysOpen)
      store <- adjust$updatedStore
      if(adjustedPositions != 0){
        adjustedPositions <- adjust$pos
      }
    }
    #If enough periods have passed 
    if(length(params$lookback) < 0)
      print("The lookback parameter has not been initialised correctly. It has a length of: 0")
    if(length(params$rsiLookback) < 0)
      print("The RSI lookback parameter has not been initialised correctly. It has a length of: 0")
    if(store$iter == 60){
      sellAll <- sellAllOpenPositions(store, seriesIndex, todaysOpen)
      store <- sellAll$store  
      adjustedPositions <- sellAll$adjustedPositions
      if(adjustedPositions)
        print("All positions have been cancelled")
    }
    if(!strategyOn){
      #If the strategy is turned off, we need to sell all of our positions
      sellAll <- sellAllOpenPositions(store, seriesIndex, todaysOpen)
      store <- sellAll$store  
      adjustedPositions <- sellAll$adjustedPositions
    }else if(store$iter > max(lookback, rsiLookback) && strategyOn){
      #Set up indicators
      rsi <- calculateRSI(series$Close, rsiLookback)
      movingAverage <- switch(params$maType,
                              SMA = calculateSMA(series$Close, params$smaLookback),
                              EMA = calculateEMA(series$Close, params$emaLookback),
                              WMA = calculateWMA(series$Close, params$wmaLookback))
      
      #Determine the trend of the series
      upTrend <- isTrendingUp(movingAverage, lookback, params$maThreshold)
      downTrend <- isTrendingDown(movingAverage, lookback, params$maThreshold)
      
      #Determine if the series is overbought or oversold
      oversold <- isRSIOversold(rsi[length(rsi)], params$oversoldThresh)
      overbought <- isRSIOverbought(rsi[length(rsi)], params$overboughtThresh)
      
      #Check if todays open is higher / lower than the previous days close
      openCloseDiff <- todaysOpen - ydaysClose
      
      #If the market is overbought and we are in an uptrend and todays open is higher than yesterdays days open
      if(overbought && upTrend && openCloseDiff > 0){
        entryPrice <- newRowList[[seriesIndex]]$Open
        tradeRecord <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "buy", limitOrders)
        store <- tradeRecord$store
        if(limitOrders){
          limitPrices1[seriesIndex] <- tradeRecord$limitPrice 
          limitOrders1[seriesIndex] <- positionSize
        } else{
          adjustedPositions <- adjustedPositions + positionSize # Buy signal 
        }
      }
      else if(oversold && downTrend && openCloseDiff < 0){#If the market is oversold and we are in a downtrend and todays open is lower than yeserdays open
        entryPrice <- newRowList[[seriesIndex]]$Open
        tradeRecord <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "sell", limitOrders)
        store <- tradeRecord$store
        if(limitOrders){
          limitPrices1[seriesIndex] <- tradeRecord$limitPrice 
          limitOrders1[seriesIndex] <- positionSize
        }else
          adjustedPositions <- adjustedPositions -positionSize #Sell signal
      }
    }
      pos[seriesIndex] <- adjustedPositions
    }
  #Update new market orders.
  marketOrders <- pos
  #-----------------------------------------------------------------------------
  #Add in for testing
  #if(sum(marketOrders) != 0){
    #print("Market orders")
    #print(marketOrders)
  #}
  #if(sum(limitOrders1) != 0){
    #print("Limit orders")
    #print(limitOrders1)
  #}
  #if(sum(limitPrices1) != 0){
    #print("Limit prices")
    #print(limitPrices1)
  #}
  #-----------------------------------------------------------------------------
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1 = limitOrders1,
              limitPrices1 = limitPrices1,
              limitOrders2 = limitOrders2,
              limitPrices2 = limitPrices2))
}
#-------------------------------------------------------------------------------
#Functions to check if the RSI is oversold or overbought

#Check if the RSI is oversold based on the oversold threshold
isRSIOversold <- function(rsi, oversoldThresh){
  rsiOversold <- FALSE
  if(rsi < oversoldThresh)
    rsiOversold <- TRUE
  
  return(rsiOversold)
}

#Check if the RSI is overbought based on the oversold threshold
isRSIOverbought <- function(rsi, overboughtThresh){
  rsiOverbought<- FALSE
  if(rsi > overboughtThresh)
    rsiOverbought <- TRUE
  
  return(rsiOverbought)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#Functions calculate the desired indicator based on a lookback
calculateRSI <- function(series, lookback){
  if(lookback < 0)
    print("Error, Lookback is empty")
  
  if(lookback == nrow(series))
    lookback <- lookback - 1
    
  rsi <- RSI(series, lookback)
  return(rsi)
}
calculateSMA <- function(series, lookback) { # n is the period
  if(nrow(series) < lookback)
    lookback <- nrow(series)
  sma <- SMA(series, lookback)
  return(sma)
}
calculateEMA <- function(series, lookback) { # n is the period
  if(nrow(series) < lookback)
    lookback <- nrow(series)
  ema <- EMA(series, lookback)
  return(ema)
}
calculateWMA <- function(series, lookback){
  if(nrow(series) < lookback)
    lookback <- nrow(series)
  
  wma <- WMA(series, lookback)
  return(wma)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Functions to determine the trend of a series

#Determine if a time series is upwardly trending based on a moving average, if threshold% of the periods in the lookback are 
# increasing in price, there is an uptrend
isTrendingUp <- function(movingAverage, lookback, threshold){
  movingAverage <- coredata(movingAverage)
  trendingScore <- 0
  isTrendingUpwards <- FALSE
  
  # Ensure there are enough data points
  if(length(movingAverage) < lookback) {
    print("Not enough data for the specified lookback")
    return(FALSE)
  }
  
  for(i in seq(from = length(movingAverage), to = (length(movingAverage) - lookback + 1), by = -1)){
    if(!is.na(movingAverage[i]) && !is.na(movingAverage[i-1]) && movingAverage[i] > movingAverage[i - 1]){
      trendingScore <- trendingScore + 1
    }
  }
  if(trendingScore >= lookback * threshold)
    isTrendingUpwards <- TRUE
  
  return(isTrendingUpwards)
}

#Determine if a time series is downwardly trending based on a moving average, if threshold% of the periods in the lookback are 
# decreasing in price, there is an downtrend
isTrendingDown <- function(movingAverage, lookback, threshold){
  movingAverage <- coredata(movingAverage)
  trendingScore <- 0
  isTrendingDownwards <- FALSE
  
  # Ensure there are enough data points
  if(length(movingAverage) < lookback) {
    print("Not enough data for the specified lookback")
    return(FALSE)
  }
  
  for(i in seq(from = length(movingAverage), to = (length(movingAverage) - lookback + 1), by = -1)){
    if(!is.na(movingAverage[i]) && !is.na(movingAverage[i-1]) && movingAverage[i] < movingAverage[i - 1]){
      trendingScore <- trendingScore + 1
    }
  }
  
  if(trendingScore >= lookback * threshold)
    isTrendingDownwards <- TRUE
  
  return(isTrendingDownwards)
}
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
#Functions for managing the store
#-------------------------------------------------------------------------------

#Initilaises the store
initStore <- function(newRowList, series) {
  ohlcvStore <- list()
  tradeRecords <- vector("list", length = 10)
  tradeHistory <- list(wins = numeric(0), losses = numeric(0))
  strategyOn <- rep(TRUE, length(series))
  totalBuys <- 0
  totalSells <- 0
  for (s in series) {
    ohlcvStore[[s]] <- xts(matrix(numeric(0), ncol = 5, dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
                           order.by = as.Date(character()))
    tradeRecords[[s]] <- list()
  }
  
  count <- rep(0, 10)
  return(list(iter = 0, ohlcv = ohlcvStore, count = count, tradeRecords = tradeRecords, tradeHistory = tradeHistory, strategyOn = strategyOn, totalBuys = totalBuys, totalSells = totalSells))
}

#Updates the values in the store
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  
  for (s in series) {
    if (!is.null(newRowList[[s]])) {
      store$ohlcv[[s]] <- rbind(store$ohlcv[[s]], newRowList[[s]])
    }
  }
  
  return(store)
}