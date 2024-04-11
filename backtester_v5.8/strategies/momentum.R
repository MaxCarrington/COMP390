#-------------------------------------------------------------------------------
#The momentum strategy here is a very simple one. If a time series is deemed to be upwardly trending, with a trending moving average and an RSI of 
#over 70 (although this value may change after parameter optimisation), this indicates strong market momentum, buy the asset if the open is higher 
#than the previous close. Conversely, If the market is downwardly trending, with a downward trending moving average and an RSI under 30 (again, this 
#value is subject to change), short an asset when the open is lower than the close. The premise of this idea is to capitalise on the momentum created 
#by price gaps. 
#-------------------------------------------------------------------------------
source('./RiskManagement/position_size_calc.R')
getOrders <- function(store, newRowList, currentPos, info, params) {
  limitOrders <- TRUE
  positionSize <- 1
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  store <- updateStore(store, newRowList, params$series)    
  marketOrders <- -currentPos; pos <- allzero
  
  for (i in 1:length(params$series)) {
    
    #Initialise values from the store, parameters and price information
    seriesIndex <- params$series[i]
    series <- head(store$ohlcv[[seriesIndex]], -1)
    holdingPeriod <- params$holdingPeriod[[i]]
    ydaysClose <- coredata(series$Close[length(series$Close)])
    todaysOpen <- coredata(newRowList[[seriesIndex]]$Open)
    
    #Check if there are any trade records
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      if(!limitOrders)#Update the entry prices in trade records as we can only use open n + 1 for record n
        store <- updateEntryPrices(store, newRowList, params$series, seriesIndex)
      else #Check if limit price has been hit
        store <- checkIfLimitPriceHit(store, newRowList, params$series, seriesIndex)
    }
    #Initialise position adjustment factor
    adjustedPositions <- 0
    #If there are any trade records 
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      #Increment all open positions by 1
      store <- incrementHoldingPeriods(store, seriesIndex)
      
      #Handle closing of orders, stop losses and take profits 
      adjust <- adjustPositions(store, seriesIndex, holdingPeriod, positionSize, todaysOpen)
      store <- adjust$updatedStore
      adjustedPositions <- adjust$pos
      }
    #If enough periods have passed 
    if(length(params$lookback[[i]]) < 0)
      print("The lookback parameter has not been initialised correctly. It has a length of: 0")
    if(length(params$rsiLookback[[i]]) < 0)
      print("The RSI lookback parameter has not been initialised correctly. It has a length of: 0")
    
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
        store <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "buy", limitOrders)
        adjustedPositions <- adjustedPositions + positionSize # Buy signal
      }
      else if(oversold && downTrend && openCloseDiff < 0){#If the market is oversold and we are in a downtrend and todays open is lower than yeserdays open
        entryPrice <- newRowList[[seriesIndex]]$Open
        store <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "sell", limitOrders) 
        adjustedPositions <- adjustedPositions + -positionSize #Sell signal
      }
    }
    pos[seriesIndex] <- adjustedPositions
  }
  #Update new market orders.
  
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
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
  sma <- SMA(series, lookback)
  return(sma)
}
calculateEMA <- function(series, lookback) { # n is the period
  ema <- EMA(series, lookback)
  return(ema)
}
calculateWMA <- function(series, lookback){
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
#Functions for managing trade records

#Update the date of the limit order to the current date as the order will be exected
checkIfLimitPriceHit <- function(store, newRowList, series, seriesIndex){
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  if(length(store$tradeRecords[[seriesIndex]]) > 0){
    for(i in 1: length(tradeRecords)){
      tradeRecord <- tradeRecords[[i]]
      entryPrice <- tradeRecord$entryPrice
      tradeDirection <- tradeRecord$tradeType
      if (is.na(tradeRecord$entryDate)) { #Ensure it is a limit order
        
        #Get todays high and low price
        todaysHigh <- coredata(newRowList[[seriesIndex]]$High)
        todaysLow <- coredata(newRowList[[seriesIndex]]$Low)
        
        if((tradeDirection == "buy") && (todaysLow < entryPrice)){
          print(paste("A buy limit order has been executed at price:", entryPrice, "on the date:", latestDate))
          tradeRecord$entryDate <- latestDate 
        }else if ((tradeDirection == "sell") && (todaysHigh > entryPrice)){
          print(paste("A sell limit order has been executed at price:", entryPrice, "on the date:", latestDate))
          tradeRecord$entryDate <- latestDate
        } else{
          print("Limit order is being removed as the price does not reflect that which was set for the limit order")
          tradeRecord$entryDate <- latestDate
          tradeRecord$cancelled <- TRUE
          tradeRecord$closed <- TRUE
        }
      } else{
        print(paste("A", tradeDirection, "limit order has already been placed at price:", entryPrice, "on the date:", latestDate))
      }
      tradeRecords[[i]] <- tradeRecord
    }
  }
  store$tradeRecords[[seriesIndex]] <- tradeRecords
  return(store)
}
#Adjusts positions based on stop losses and take profits and closes any positions
adjustPositions <- function(store, seriesIndex, holdingPeriod, positionSize, todaysOpen){
  
  adjustedPositions <- 0
  #Check if we should close any positions based on the lookback
  close <- checkClosePositions(store, seriesIndex, holdingPeriod, positionSize, todaysOpen)
  store <- close$store
  #print(store$tradeRecords)
  if(close$position){# If we need to close the position
    ifelse(close$tradeType == "buy",
           adjustedPositions <-  -close$size, # Close the buy position by selling off
           adjustedPositions <- close$size)
  }else{
    takeProfits <- checkTakeProfits(store, todaysOpen, seriesIndex)
    if(length(takeProfits) > 0){
      adjustedPositions <- adjustedPositions + sum(takeProfits)
      print("Take profit hit. Exiting position and taking profit...")
    }
  }
  stopLosses <- checkStopLossesHit(store, todaysOpen, seriesIndex)
  if(length(stopLosses) > 0){
    adjustedPositions <- adjustedPositions + sum(stopLosses)
    print("Stop Loss hit. Exiting position...")
  }
  return(list(updatedStore = store, pos = adjustedPositions))
}
#This function checks if stop losses have been hit, if they have, add them to a 
# position so the trade is cancelled
checkStopLossesHit <- function(store, todaysOpen, seriesIndex){
  positionSize <- c()
  tradeRecord <- store$tradeRecords[[seriesIndex]]
  for(i in 1:length(tradeRecord)){
    tradeEntryPrice <- tradeRecord[[i]]$entryPrice
    stopLoss <- tradeRecord[[i]]$stopLoss
    orderType <- tradeRecord[[i]]$tradeType
    if(orderType == "buy" && todaysOpen <= stopLoss){
      positionSize <- c(positionSize, -tradeRecord[[i]]$positionSize)
      
    } else if(orderType == "sell" && todaysOpen >= stopLoss){
      positionSize <- c(positionSize, tradeRecord[[i]]$positionSize)
    }
  }
  return(positionSize)
}
#This function checks if take profits have been hit, if they have, add them to 
# a position to take profit
checkTakeProfits <- function(store, todaysOpen, seriesIndex){
  positionSize <- c()
  tradeRecord <- store$tradeRecords[[seriesIndex]]
  for(i in 1:length(tradeRecord)){
    tradeEntryPrice <- tradeRecord[[i]]$entryPrice
    takeProfit <- tradeRecord[[i]]$takeProfit
    orderType <- tradeRecord[[i]]$tradeType
    if(orderType == "buy" && todaysOpen >= takeProfit){
      positionSize <- c(positionSize, -tradeRecord[[i]]$positionSize)
      
    } else if(orderType == "sell" && todaysOpen <= takeProfit){
      positionSize <- c(positionSize, tradeRecord[[i]]$positionSize)
    }
    return(positionSize)
  }
}
kellyFormulaPosSize <- function(positionSize, store, info, todaysOpen){
  #Determine position size based on the kelly formula-
  positionRatio <- 0.1 #Default position size
  if(length(store$tradeHistory$wins) + length(store$tradeHistory$losses) > 0){
    analysePreviousTrades(store$tradeHistory)
    positionRatio <- calculatePositionSize() #The position size is dynamically calcualted based on the Kelly formula
  }
  
  positionSize <- info$balance * positionRatio
  positionSize <- floor(positionSize / todaysOpen)
  
}
#Improve
calculateTakeProfit <- function(tradeType, entryPrice){
  #priceChange <- priceChangeSinceHalfLife  MAYBE IMPLEMENT THIS LATER
  if(tradeType == "buy"){
    exitPrice <- entryPrice * 1.25
  }else{ #Must be a sell
    exitPrice <- entryPrice * 0.75
  }
  return(coredata(exitPrice))
}
priceChangeSinceHalfLife <- function(){
  
}
#Set to half of the take profit so far
calculateStopLoss <- function(tradeType, entryPrice){
  if(tradeType == "buy"){
    stopLoss <- entryPrice * 0.875
  }else{ #Must be a sell
    stopLoss <- entryPrice * 1.125
  }
  return(coredata(stopLoss))
}
#Funciton to calculate limit price orders
calculateLimitPrice <- function(tradeType, entryPrice, percentage = 0.005){
  if(tradeType == "buy"){
    limitPrice <- entryPrice * (1- percentage)
  }else if(tradeType == "sell"){
    limitPrice <- entryPrice * (1 + percentage)
  } else{
    stop("Invalid trade type. Must be either 'buy' or 'sell'.")
  }
  return(coredata(limitPrice))
}
# Keeps a record of trades, this is used in positionSizing
createTradeRecord <- function(store, seriesIndex, positionSize, entryPrice, tradeType, limitOrder) {
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  limitPrice <- calculateLimitPrice(tradeType, entryPrice)
  if(limitOrder){
    tradeRecord <- list(
      entryDate = NA,
      entryPrice = limitPrice,
      positionSize = positionSize,
      tradeType = tradeType, # Record whether it's a buy or sell trade
      closed = FALSE,
      exitDate = NULL,
      halfLifeHoldingPeriod = 0,
      exitPrice = 0,
      takeProfit = calculateTakeProfit(tradeType, limitPrice),
      stopLoss = calculateStopLoss(tradeType, limitPrice),
      cancelled = FALSE
    )
  }else{
    tradeRecord <- list(
      entryDate = coredata(latestDate),
      entryPrice = NA,
      positionSize = positionSize,
      tradeType = tradeType, # Record whether it's a buy or sell trade
      closed = FALSE,
      exitDate = NULL,
      halfLifeHoldingPeriod = 0,
      exitPrice = 0,
      takeProfit = 0,
      stopLoss = 0,
      cancelled = FALSE
    )
    
  }
  
  store$tradeRecords[[seriesIndex]] <- c(store$tradeRecords[[seriesIndex]], list(tradeRecord))
  return(store)
}
#Closes a trade record
closeTradeRecord <- function(store, seriesIndex, tradeRecord, exitDate, exitPrice, positionSize){
  slippagePercent = 0.2
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  
  for(i in 1:length(tradeRecords)){
    if(!tradeRecord$cancelled){
      if(tradeRecords[[i]]$entryDate == tradeRecord$entryDate){
        tradeRecord$closed <- TRUE
        tradeRecord$exitPrice <- exitPrice
        tradeRecord$exitDate <- latestDate
        tradeRecords[[i]] <- tradeRecord
      }
    }
  }
  store$tradeRecords[[seriesIndex]] <- tradeRecords
  if(!tradeRecord$cancelled){
    profit <- ifelse(tradeRecord$tradeType == "buy", 
                     ((exitPrice * (1 - slippagePercent) - tradeRecord$entryPrice) * positionSize), #Long
                     ((exitPrice * (1 + slippagePercent) - tradeRecord$exitPrice) * positionSize))#Short
    store <- updateTradeHistory(store, profit)
  }
  
  return(store)
}
# Update the entry prices as we only find out a market orders price the following day, 
# also determine stop loss and take profit
updateEntryPrices <- function(store, newRowList, series, seriesIndex) {
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  if(length(tradeRecords) > 0){
    for(i in 1:length(tradeRecords)){
      tradeRecord <- tradeRecords[[i]]
      # Check if entryPrice needs updating (if it's NA)
      if (is.na(tradeRecord$entryPrice)) {
        todaysOpen <- coredata(newRowList[[seriesIndex]]$Open)
        tradeRecord$entryPrice <- todaysOpen
        tradeType <- tradeRecord$tradeType
        tradeRecord$takeProfit <- calculateTakeProfit(tradeType, todaysOpen)
        tradeRecord$stopLoss <- calculateStopLoss(tradeType, todaysOpen)
        tradeRecords[[i]] <- tradeRecord
      }
    }
    store$tradeRecords[[seriesIndex]] <- tradeRecords
  }
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
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  for(i in 1:length(tradeRecords)) {
    tradeRecord <- tradeRecords[[i]]
    if(!tradeRecord$closed)
      tradeRecord$halfLifeHoldingPeriod <- tradeRecord$halfLifeHoldingPeriod + 1
    tradeRecords[[i]] <- tradeRecord
  }
  store$tradeRecords[[seriesIndex]] <- tradeRecords
  return(store)
}
#Check if enough days have passed to close a position
checkClosePositions <- function(store, seriesIndex, halfLifeHoldingPeriod, positionSize, todaysOpen, currentPos){
  position <- FALSE
  type <- ""
  size <- 0
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  for(i in 1:length(store$tradeRecords[[seriesIndex]])){
    tradeRecord <- store$tradeRecords[[seriesIndex]][[i]]
    if(!tradeRecord$closed){
      posDuration <- tradeRecord$halfLifeHoldingPeriod
      if(posDuration >= halfLifeHoldingPeriod){
        #Close the position
        #Mark the tradeRecord as closed
        exitDate <- latestDate
        positionSize <- tradeRecord$positionSize
        type <- tradeRecord$tradeType
        store <- closeTradeRecord(store, seriesIndex, tradeRecord, exitDate, todaysOpen, positionSize)
        position = TRUE
      }
    }
  }
  return(list(store = store, position = position, size = positionSize, tradeType = type))
}
#-------------------------------------------------------------------------------

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
  
  return(list(iter = 0, ohlcv = ohlcvStore, count = count, tradeRecords = tradeRecords, tradeHistory = tradeHistory, limitOrderIDs = 0))
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