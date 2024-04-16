#-------------------------------------------------------------------------------
# Functions to manage records of trades 
#-------------------------------------------------------------------------------
source('./RiskManagement/stop_loss.R')
source('./RiskManagement/take_profit.R')
source('./RiskManagement/limit_orders.R')
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
  return(list(store = store, limitPrice = limitPrice))
}
#Adjusts positions based on stop losses and take profits and closes any positions
adjustPositions <- function(store, seriesIndex, holdingPeriod, positionSize, todaysOpen){
  adjustedPositions <- 0
  #Check if we should close any positions based on the lookback
  close <- checkClosePositions(store, seriesIndex, holdingPeriod, positionSize, todaysOpen)
  store <- close$store
  
  if(close$position){# If we need to close the position
    ifelse(close$tradeType == "buy",
           adjustedPositions <-  -close$size, # Close the buy position by selling off
           adjustedPositions <- close$size)
    print(paste("Close position",adjustedPositions))
  }else{
    takeProfits <- checkTakeProfits(store, todaysOpen, seriesIndex)
    positions <- takeProfits$positionSize
    store <- takeProfits$store
    if(sum(positions) != 0){
      adjustedPositions <- adjustedPositions + sum(positions)
      print(paste("TP adjusted Positions", adjustedPositions))
    }
  }
  stopLosses <- checkStopLossesHit(store, todaysOpen, seriesIndex)
  positions <- stopLosses$positionSize
  store <- stopLosses$store
  if(sum(positions) != 0){
    adjustedPositions <- adjustedPositions + sum(positions)
    print(paste("SL adjusted Positions", adjustedPositions))
  }
  return(list(updatedStore = store, pos = adjustedPositions))
}
#Closes a trade record
#Closes a trade record and updates trade history
closeTradeRecord <- function(store, seriesIndex, tradeRecord){
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  
  for(i in 1:length(tradeRecords)){
    if(tradeRecords[[i]]$entryDate == tradeRecord$entryDate && !tradeRecords[[i]]$closed){
      tradeRecords[[i]]$closed <- TRUE
      tradeRecords[[i]]$exitPrice <- NA
      tradeRecords[[i]]$exitDate <- latestDate
    }
  }
  store$tradeRecords[[seriesIndex]] <- tradeRecords
  return(store)
}

#Adds the exit price of a trade to the trade record, the exit price will be a market order and depending on the price
# will either be 
addExitPrice <- function(store, seriesIndex, newRowList){
  series <- store$ohlcv[[seriesIndex]]
  ydaysClose <- coredata(series$Close[length(series$Close)])
  todaysOpen <- coredata(newRowList[[seriesIndex]]$Open)
  
  slippage = abs(ydaysClose - todaysOpen) * 0.2
  
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  for(i in 1:length(tradeRecords)){
    tradeRecord <- tradeRecords[[i]]
    positionSize <- tradeRecord$positionSize
    if(tradeRecord$closed){
      tradeRecord$exitPrice <- todaysOpen
      # Calculate profit or loss
      profit <- ifelse(tradeRecord$tradeType == "buy", 
                       ((ydaysClose - tradeRecord$entryPrice) * positionSize - slippage), # Long
                       ((tradeRecord$entryPrice - ydaysClose) * positionSize) + slippage) # Short
      # Update trade history
      if(profit > 0) {
        store$tradeHistory$wins <- c(store$tradeHistory$wins, profit)
      } else{
        store$tradeHistory$losses <- c(store$tradeHistory$losses, profit)
      }
    }
    tradeRecords[[i]] <- tradeRecord
  }
  store$tradeRecords[[seriesIndex]] <- tradeRecords
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
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  for(i in 1:length(tradeRecords)){
    tradeRecord <- tradeRecords[[i]]
    if(!tradeRecord$closed){
      posDuration <- tradeRecord$halfLifeHoldingPeriod
      if(posDuration >= halfLifeHoldingPeriod){
        #Close the position
        #Mark the tradeRecord as closed
        exitDate <- latestDate
        positionSize <- tradeRecord$positionSize
        type <- tradeRecord$tradeType
        store <- closeTradeRecord(store, seriesIndex, tradeRecord)
        position = TRUE
      }
    }
  }
  return(list(store = store, position = position, size = positionSize, tradeType = type))
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
          #print(paste("A buy limit order has been executed at price:", entryPrice, "on the date:", latestDate, "because it is greater than todays low:", todaysLow, ". Executed on the date:", latestDate))
          tradeRecord$entryDate <- latestDate 
        }else if ((tradeDirection == "sell") && (todaysHigh > entryPrice)){
          #print(paste("A sell limit order has been executed at price:", entryPrice, "because it is less than todays high:", todaysHigh, ". Executed on the date:", latestDate))
          tradeRecord$entryDate <- latestDate
        } else{
          #print("Limit order is being removed as the price does not reflect that which was set for the limit order")
          tradeRecord$entryDate <- latestDate
          tradeRecord$cancelled <- TRUE
          tradeRecord$closed <- TRUE
        }
      } #else{
        #print(paste("A", tradeDirection, "limit order has already been placed at price:", entryPrice, "on the date:", latestDate))
      #}
      tradeRecords[[i]] <- tradeRecord
    }
  }
  store$tradeRecords[[seriesIndex]] <- tradeRecords
  return(store)
}
sellAllOpenPositions <- function(store, seriesIndex, exitPrice){
  adjustedPositions <- 0
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  if(length(tradeRecords) > 0){
    adjustedPositions <- 0
    exitDate <- index(last(store$ohlcv[[seriesIndex]]))
    for(i in 1:length(tradeRecords)){
      tradeRecord <- tradeRecords[[i]]
      tradeType <- tradeRecord$tradeType
      type <- tradeRecord$tradeType
      closed <- tradeRecord$closed
      positionSize <- tradeRecord$positionSize
      if(!closed){
        if(type == "buy"){
          adjustedPositions <- adjustedPositions + positionSize
          store <- closeTradeRecord(store, seriesIndex, tradeRecord)
        } else{
          adjustedPositions <- adjustedPositions - positionSize 
          store <- closeTradeRecord(store, seriesIndex, tradeRecord)
        }
      }
    }
  }
  return(list(adjustedPositions = adjustedPositions, store = store))
}