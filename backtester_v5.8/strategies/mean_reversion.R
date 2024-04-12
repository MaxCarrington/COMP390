#-------------------------------------------------------------------------------
# Mean-reversion strategy that uses Bollinger Bands, trades will be executed when t
# the price crosses the Bollinger Bands thresholds, expecting a reversal towards the mean.
# The key importance of this strategy is in the data analysis. The data analysis determines
# half lives and then this is used to determine holding periods for mean reverison.
#-------------------------------------------------------------------------------
source('./RiskManagement/position_size_calc.R')
source('./RiskManagement/trade_record_management.R')
getOrders <- function(store, newRowList, currentPos, info, params) {
  limitOrders <- TRUE
  positionSize <- 1
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  #Initialise the store if it is not already initialised
  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  #Update the store 
  store <- updateStore(store, newRowList, params$series)
  #Initialise all positions to 0
  pos <- allzero
  
  #Set market orders to try and cancel out current positions 
  #marketOrders <- -currentPos;
  
  #-----------------------------------------------------------------------------
  if(store$iter == 558){
    print("Wins:")
    print(length(store$tradeHistory$wins))
    print("Losses")
    print(length(store$tradeHistory$losses))
    print("Number of trades: ")
    print(store$tradeCount)
    print(info)
  }
  if(store$iter == 558){
    for (x in 1:length(params$series)){
      seriesIndex <- params$series[x]
      print(length(store$tradeRecords[[seriesIndex]]))
    }
  }
  #-----------------------------------------------------------------------------
  
  #Iterate through each suitable series.
  for (i in 1:length(params$series)) {
    #Get information about which series, the half Life duration and todays open / yesterdays close
    seriesIndex <- params$series[i]
    series <- head(store$ohlcv[[seriesIndex]], -1)
    halfLife <- params$halfLives[i]
    ydaysClose <- series$Close[length(series$Close)]
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
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      #Increment all open positions by 1
      store <- incrementHoldingPeriods(store, seriesIndex)
      
      #Handle closing of orders, stop losses and take profits 
      close <- checkClosePositions(store, seriesIndex, halfLife, positionSize, todaysOpen)
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
          print("Take Profit hit. Adjusting Position....")
        }
      }
      stopLosses <- checkStopLossesHit(store, todaysOpen, seriesIndex)
      if(length(stopLosses) > 0)
        adjustedPositions <- adjustedPositions + sum(stopLosses)
        print("Stop Loss hit. Adjusting Position....")
    }
    
    if (store$iter > halfLife) {
      #Ensure we are not using todays data as we would not have access to this
      hlcPrices <- head(store$ohlcv[[seriesIndex]][, c("High", "Low", "Close")], -1)
      bbands <- calculateBollingerBands(hlcPrices, halfLife, params$stdDev)
      #Buy if the bands suggest the close is below the bollinger band
      if(ydaysClose < bbands[,"dn"]){
        entryPrice <- newRowList[[seriesIndex]]$Open
        store <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "buy", halfLife)
        store$tradeCount <- store$tradeCount + 1
        pos[params$series[i]] <- positionSize
      }
      else if (ydaysClose < bbands[,"up"]){ #sell if the bands suggest the close is above the bollinger band
        entryPrice <- newRowList[[seriesIndex]]$Open
        store <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "sell", halfLife)
        store$tradeCount <- store$tradeCount + 1
        pos[params$series[i]] <- -positionSize
      }
    }
  }
  marketOrders <- pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
#-------------------------------------------------------------------------------
# Function to calculate Bollinger bands based on a lookback and standard deviation
calculateBollingerBands <- function(hlcPrices, lookback, stdDev) {
  
  if (is.xts(hlcPrices) && all(c("High", "Low", "Close") %in% colnames(hlcPrices))) {
    bbands <- BBands(HLC = hlcPrices, n = lookback, sd = stdDev)
    return(bbands)
  } else {
    stop("hlcPrices must be an xts object with High, Low, and Close columns.")
  }
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Functions for managing the store

#Initialises the store
initStore <- function(newRowList, series) {
  ohlcvStore <- list()
  tradeRecords <- vector("list", length = 10) # Ensure tradeRecords is large enough
  tradeHistory <- list(wins = numeric(0), losses = numeric(0))
  tradeCount <- 0
  for (s in series) {
    ohlcvStore[[s]] <- xts(matrix(numeric(0), ncol = 5, dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
                           order.by = as.Date(character()))
  }
  count <- vector(mode = "numeric", length = length(series))
  
  return(list(iter = 0, ohlcv = ohlcvStore, count = count, tradeRecords = tradeRecords, tradeHistory = tradeHistory, tradeCount = tradeCount))
}

#Updates the store
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  
  for (s in series) {
    newDataRow <- newRowList[[s]]
    
    if (!is.null(newDataRow) && inherits(newDataRow, "xts")) {
      store$ohlcv[[s]] <- rbind(store$ohlcv[[s]], newDataRow)
    } else {
      warning(paste("Data for series", s, "is not in correct xts format or is missing. Skipping update for this series."))
    }
  }
  
  return(store)
}
#-------------------------------------------------------------------------------