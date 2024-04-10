#-------------------------------------------------------------------------------
#The momentum strategy here is a very simple one. If a time series is deemed to be upwardly trending, with a trending moving average and an RSI of 
#over 70 (although this value may change after parameter optimisation), this indicates strong market momentum, buy the asset if the open is higher 
#than the previous close. Conversely, If the market is downwardly trending, with a downward trending moving average and an RSI under 30 (again, this 
#value is subject to change), short an asset when the open is lower than the close. The premise of this idea is to capitalise on the momentum created 
#by price gaps. 
#-------------------------------------------------------------------------------
source('./RiskManagement/position_size_calc.R')
getOrders <- function(store, newRowList, currentPos, info, params) {
  positionSize <- 1
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  store <- updateStore(store, newRowList, params$series)    
  marketOrders <- -currentPos; pos <- allzero
  
  for (i in 1:length(params$series)) {
    #Decide a 
    
    positionRatio <- 0.1 # Default position ratio if no history available
    if(length(store$tradeHistory$wins) + length(store$tradeHistory$losses) > 0){
      analysePreviousTrades(store$tradeHistory)
      positionRatio <- calculatePositionSize() #The position size is dynamically calcualted based on the Kelly formula
    }
    
    seriesIndex <- params$series[i]
    series <- head(store$ohlcv[[seriesIndex]], -1)
    
    ydaysClose <- coredata(series$Close[length(series$Close)])
    todaysOpen <- coredata(newRowList[[seriesIndex]]$Open)
    
    #Using the kelly formula, the size of the position is calculated by taking the amount of balance 
    #to invest divided by the number of assets 
    
    positionSize <- info$balance * positionRatio
    positionSize <- floor(positionSize / todaysOpen)
    # Increment holding period for each open position
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      #Increment all open positions by 1
      store <- incrementHoldingPeriods(store, seriesIndex)
      #Check if we should close any positions based on the lookback
      close <- checkClosePositions(store, seriesIndex, params$holdingPeriod[[i]], positionSize, todaysOpen)
      store <- close$store
      #print(store$tradeRecords)
      if(close$position)
        pos[seriesIndex] <- -currentPos[seriesIndex] # Close the position
    }
    
    #If enough periods have passed 
    if (store$iter > max(params$lookback[[i]], params$rsiLookback[[i]])){
      #Set up indicators
      rsi <- calculateRSI(series$Close, params$rsiLookback)
      movingAverage <- switch(params$maType,
                              SMA = calculateSMA(series$Close, params$smaLookback),
                              EMA = calculateEMA(series$Close, params$emaLookback),
                              WMA = calculateWMA(series$Close, params$wmaLookback))
      
      #Determine the trend of the series
      upTrend <- isTrendingUp(movingAverage, params$lookback, params$maThreshold)
      downTrend <- isTrendingDown(movingAverage, params$lookback, params$maThreshold)
      
      #Determine if the series is overbought or oversold
      oversold <- isRSIOversold(rsi[length(rsi)], params$oversoldThresh)
      overbought <- isRSIOverbought(rsi[length(rsi)], params$overboughtThresh)
      #Check if todays open is higher / lower than the previous days close
      openCloseDiff <- todaysOpen - ydaysClose
      
      #If the market is overbought and we are in an uptrend and todays open is higher than yesterdays days open
      if(overbought && upTrend && openCloseDiff > 0){
        entryPrice <- newRowList[[seriesIndex]]$Open
        store <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "buy")
        pos[seriesIndex] <- positionSize # Buy signal
      }
      else if(oversold && downTrend && openCloseDiff < 0){#If the market is oversold and we are in a downtrend and todays open is lower than yeserdays open
        entryPrice <- newRowList[[seriesIndex]]$Open
        store <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "sell") 
        pos[seriesIndex] <- -positionSize #Sell signal
      } else{
        pos[seriesIndex] <- 0
      } 
    }
  }
  #Update new market orders.
  
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}

# Keeps a record of trades, this is used in positionSizing
createTradeRecord <- function(store, seriesIndex, positionSize, entryPrice, tradeType) {
  
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))

  tradeRecord <- list(
    entryDate = latestDate,
    entryPrice = entryPrice,
    positionSize = positionSize,
    tradeType = tradeType, # Record whether it's a buy or sell trade
    closed = FALSE,
    exitDate = NULL,
    holdingPeriod = 0,
    exitPrice = 0
  )
  store$tradeRecords[[seriesIndex]] <- c(store$tradeRecords[[seriesIndex]], list(tradeRecord))
  return(store)
}
#Closes a trade record
closeTradeRecord <- function(store, seriesIndex, tradeRecord, exitDate, exitPrice, positionSize){
  slippagePercent = 0.2
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  
  for(i in 1:length(tradeRecords)){
    
    if(tradeRecords[[i]]$entryDate == tradeRecord$entryDate){
      tradeRecord$closed <- TRUE
      tradeRecord$exitPrice <- exitPrice
      tradeRecord$exitDate <- latestDate
      tradeRecords[[i]] <- tradeRecord
    }
  }
  store$tradeRecords[[seriesIndex]] <- tradeRecords
  profit <- ifelse(tradeRecord$tradeType == "buy", 
                   ((exitPrice * (1 - slippagePercent) - tradeRecord$entryPrice) * positionSize), #Long
                   ((exitPrice * (1 + slippagePercent) - tradeRecord$exitPrice) * positionSize))#Short
  store <- updateTradeHistory(store, profit)
  return(store)
}
#Update trade history with information from profit and loss 
updateTradeHistory <- function(store, profit) {
  if (profit > 0) {
    store$tradeHistory$wins <- c(store$tradeHistory$wins, profit)
  } else if (profit < 0) {
    store$tradeHistory$losses <- c(store$tradeHistory$losses, profit)
  }
  return(store)
}
#Increment each of the trade records holding period by 1 as we are in a new day
incrementHoldingPeriods <- function(store, seriesIndex){
  for(i in 1:length(store$tradeRecords[[seriesIndex]])) {
    #print(store$tradeRecords[[seriesIndex]][[i]]$closed)
    
    if(!store$tradeRecords[[seriesIndex]][[i]]$closed)
      store$tradeRecords[[seriesIndex]][[i]]$holdingPeriod <- store$tradeRecords[[seriesIndex]][[i]]$holdingPeriod + 1
      #print(store$tradeRecords[[seriesIndex]][[i]]$holdingPeriod)
    }
  return(store)
}
#Check if enough days have passed to close a position
checkClosePositions <- function(store, seriesIndex, holdingPeriod, positionSize, todaysOpen, currentPos){
  position <- FALSE
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  for(i in 1:length(store$tradeRecords[[seriesIndex]])){
    tradeRecord <- store$tradeRecords[[seriesIndex]][[i]]
    
    if(!tradeRecord$closed){
      
      posDuration <- tradeRecord$holdingPeriod
      
      if(posDuration >= holdingPeriod){
        #Close the position
        #Mark the tradeRecord as closed
        exitDate <- latestDate
        store <- closeTradeRecord(store, seriesIndex, tradeRecord, exitDate, todaysOpen, positionSize)
        
        position = TRUE
      }
    }
  }
  return(list(store = store, position = position))
}

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
#Below functions calculate the desired indicator based on a lookback

calculateRSI <- function(series, lookback){
  if(lookback < 0)
    print("Error, Lookback is empty")
  print(lookback)
  if(lookback == nrow(series))
    lookback <- lookback - 1
    
  rsi <- RSI(series, lookback)
  return(rsi)
}
calculateSMA <- function(series, lookback) { # n is the period
  sma <- SMA(series, lookback)
  return(sma)
}

calculateEMA <- function(series, lookback) { # n is the period
  ema <- EMA(series, lookback)
  return(ema)
}
#-------------------------------------------------------------------------------


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
#Functions for managing the store

#Initilaises the store
initStore <- function(newRowList, series) {
  ohlcvStore <- list()
  tradeRecords <- vector("list", length = 10)
  tradeHistory <- list(wins = numeric(0), losses = numeric(0))
  for (s in series) {
    ohlcvStore[[s]] <- xts(matrix(numeric(0), ncol = 5, dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
                           order.by = as.Date(character()))
    tradeRecords[[s]] <- list()
  }
  
  count <- rep(0, 10)
  
  return(list(iter = 0, ohlcv = ohlcvStore, count = count, tradeRecords = tradeRecords, tradeHistory = tradeHistory))
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
#-------------------------------------------------------------------------------