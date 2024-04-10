#-------------------------------------------------------------------------------
# Mean-reversion strategy that uses Bollinger Bands, trades will be executed when t
# the price crosses the Bollinger Bands thresholds, expecting a reversal towards the mean.
# The key importance of this strategy is in the data analysis. The data analysis determines
# half lives and then this is used to determine holding periods for mean reverison.
#-------------------------------------------------------------------------------
# Mean reversion source file
getOrders <- function(store, newRowList, currentPos, info, params) {
  
  positionSize <- 1
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors

  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  store <- updateStore(store, newRowList, params$series)    
  marketOrders <- -currentPos; pos <- allzero
  if(store$iter == 558){
    print("Wins:")
    print(length(store$tradeHistory$wins))
    print("Losses")
    print(length(store$tradeHistory$losses))
    print("Number of trades: ")
    print(store$tradeCount)
  }
  if(store$iter == 558){
    for (x in 1:length(params$series)){
      seriesIndex <- params$series[x]
      print(length(store$tradeRecords[[seriesIndex]]))
    }
  }

  #Iterate through each suitable series.
  for (i in 1:length(params$series)) {
    #Get information about which series, the half Life duration and todays open / yesterdays close
    seriesIndex <- params$series[i]
    halfLife <- params$halfLives[i]
    todaysOpen <- coredata(newRowList[[seriesIndex]]$Open)
    yesterdaysCl <- tail(store$ohlcv[[seriesIndex]]$Close, 2)[1]
    
    #Update the entry prices in trade records as we can only use open n + 1 for record n
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      store <- updateEntryPrices(store, newRowList, params$series)
    }
    positionSize <- kellyFormulaPosSize(positionSize, store, info, todaysOpen)
    
    #If enough periods have passed.
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      #Increment all open positions by 1
      store <- incrementhalfLifeHoldingPeriods(store, seriesIndex)
      #Check if we should close any positions based on the lookback
      close <- checkClosePositions(store, seriesIndex, halfLife, positionSize, todaysOpen)
      store <- close$store
      #print(store$tradeRecords)
      if(close$position){# If we need to close the position
        pos[seriesIndex] <- -currentPos[seriesIndex] # Close the position
        
      }
      
       
    }
    if(halfLife < 0)
      print("Error: Half life is less than 0")
    if (store$iter > halfLife) {
      #Ensure we are not using todays data as we would not have access to this
      hlcPrices <- head(store$ohlcv[[seriesIndex]][, c("High", "Low", "Close")], -1)
      bbands <- calculateBollingerBands(hlcPrices, halfLife, params$stdDev)
      #Buy if the bands suggest the close is below the bollinger band
      if(yesterdaysCl < bbands[,"dn"]){
        entryPrice <- newRowList[[seriesIndex]]$Open
        store <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "buy", halfLife)
        store$tradeCount <- store$tradeCount + 1
        pos[params$series[i]] <- positionSize
      }
      else if (yesterdaysCl < bbands[,"up"]){ #sell if the bands suggest the close is above the bollinger band
        entryPrice <- newRowList[[seriesIndex]]$Open
        store <- createTradeRecord(store, seriesIndex, positionSize, entryPrice, "sell", halfLife)
        store$tradeCount <- store$tradeCount + 1
        pos[params$series[i]] <- -positionSize
      }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
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
#Improve
calculateTakeProfit <- function(tradeType, entryPrice){
  #priceChange <- priceChangeSinceHalfLife  MAYBE IMPLEMENT THIS LATER
  if(tradeType == "buy"){
    exitPrice <- entryPrice * 1.5
  }else{ #Must be a sell
    exitPrice <- entryPrice * 0.5
  }
  return(exitPrice)
}

priceChangeSinceHalfLife <- function(){
  
}
# Keeps a record of trades, this is used in positionSizing
createTradeRecord <- function(store, seriesIndex, positionSize, entryPrice, tradeType, halfLife) {
  
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  
  tradeRecord <- list(
    entryDate = latestDate,
    entryPrice = NA,
    positionSize = positionSize,
    tradeType = tradeType, # Record whether it's a buy or sell trade
    closed = FALSE,
    exitDate = NULL,
    halfLifeHoldingPeriod = 0,
    exitPrice = 0,
    takeProfit = 0
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
  #print(paste("Trade type:", tradeRecord$tradeType))
  #print(paste("Entry price:", tradeRecord$entryPrice, "Exit Price:", exitPrice * (1 - slippagePercent)))
  store <- updateTradeHistory(store, profit)
  return(store)
}
#Update the entry prices as we only find out a market orders price the following day
updateEntryPrices <- function(store, newRowList, series) {
  for(i in 1:length(series)){
    seriesIndex <- series[i]
    tradeRecord <- store$tradeRecords[[seriesIndex]]
    if(length(store$tradeRecords[[seriesIndex]]) > 0){
      for(x in 1: length(tradeRecord)){
        tradeRecord <- store$tradeRecords[[seriesIndex]][[x]]
        # Check if entryPrice needs updating (if it's NA)
        if (is.na(tradeRecord$entryPrice)) {
          todaysOpen <- coredata(newRowList[[seriesIndex]]$Open)
          tradeRecord$entryPrice <- todaysOpen
          tradeType <- tradeRecord$tradeType
          tradeRecord$takeProfit <- calculateTakeProfit(tradeType, todaysOpen)
          store$tradeRecords[[seriesIndex]][[x]] <- tradeRecord
        }
      }
    }
    
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
incrementhalfLifeHoldingPeriods <- function(store, seriesIndex){
  for(i in 1:length(store$tradeRecords[[seriesIndex]])) {
    #print(store$tradeRecords[[seriesIndex]][[i]]$closed)
    
    if(!store$tradeRecords[[seriesIndex]][[i]]$closed)
      store$tradeRecords[[seriesIndex]][[i]]$halfLifeHoldingPeriod <- store$tradeRecords[[seriesIndex]][[i]]$halfLifeHoldingPeriod + 1
    #print(store$tradeRecords[[seriesIndex]][[i]]$halfLifeHoldingPeriod)
  }
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