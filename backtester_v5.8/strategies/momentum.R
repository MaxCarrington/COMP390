#The momentum strategy here is a very simple one. If a time series is deemed to be upwardly trending, with a trending moving average and an RSI of 
#over 70 (although this value may change after parameter optimisation), this indicates strong market momentum, buy the asset if the open is higher 
#than the previous close. Conversely, If the market is downwardly trending, with a downward trending moving average and an RSI under 30 (again, this 
#value is subject to change), short an asset when the open is lower than the close. The premise of this idea is to capitalise on the momentum created 
#by price gaps. 

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  store <- updateStore(store, newRowList, params$series)    
  marketOrders <- -currentPos; pos <- allzero
  
  for (i in 1:length(params$series)) {
    seriesIndex <- params$series[i]
    series <- store$ohlcv[[seriesIndex]]
    #If there have been enough periods for the lookback
    if (store$iter > params$lookback[[i]]){
      if(store$count[seriesIndex] >= params$holdingPeriod[[i]] || store$count[seriesIndex] <= -params$holdingPeriod[[i]]){
        pos[seriesIndex] <- -currentPos[seriesIndex] # Close the position
        store$count[seriesIndex] <- 0 # Reset the count for this series
      } else {
        
        #Set up indicators
        rsi <- calculateRSI(series$Close, params$rsiLookback)
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
        
        ydaysClose <- coredata(series$Close[length(series$Close)])
        todaysOpen <- coredata(newRowList[[seriesIndex]]$Open)
        #Check if todays open is higher / lower than the previous days close
        openCloseDiff <- todaysOpen - ydaysClose
        
        #If the market is overbought and we are in an uptrend and todays open is higher than the previous days open
        
        if(overbought && upTrend && openCloseDiff > 0){
          print("Overbought")
          pos[seriesIndex] <- 1 # Buy signal
          store$count[seriesIndex] <- store$count[seriesIndex] + 1 # Increment the holding count
        }
        else if(oversold && downTrend && openCloseDiff < 0){
          print("Oversold")
          pos[seriesIndex] <- -1 #Sell signal
          store$count[seriesIndex] <- store$count[seriesIndex] - 1 # Decrement the holding count
        } else{
          pos[seriesIndex] <- 0
        }  
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

isRSIOversold <- function(rsi, oversoldThresh){
  rsiOversold <- FALSE
  if(rsi < oversoldThresh)
    rsiOversold <- TRUE
  
  return(rsiOversold)
}

isRSIOverbought <- function(rsi, overboughtThresh){
  rsiOverbought<- FALSE
  if(rsi > overboughtThresh)
    rsiOverbought <- TRUE
  
  return(rsiOverbought)
}

calculateRSI <- function(series, lookback){
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
#Determine if a time series is upwardly trending based on a moving average, if threshold% of the periods in the lookback are 
# increasing in price, there is an uptrend
isTrendingUp <- function(movingAverage, lookback, threshold){
  movingAverage <- coredata(movingAverage)
  trendingScore <- 0
  isTrendingUpwards <- FALSE
  # Start from the last value and go back by lookback periods
  for(i in (length(movingAverage) - lookback + 1):length(movingAverage)){
    if(movingAverage[i] > movingAverage[i - 1]) {
      trendingScore <- trendingScore + 1
    }
  }
  # Check if the score meets the upward trend criteria, currently 80%
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
  
  # Start from the last value and go back by lookback periods
  for(i in (length(movingAverage) - lookback + 1):length(movingAverage)){
    if(movingAverage[i] < movingAverage[i - 1]) {
      trendingScore <- trendingScore + 1
    }
  }
  # Check if the score meets the downward trend criteria
  if(trendingScore >= lookback * threshold)
    isTrendingDownwards <- TRUE
  
  return(isTrendingDownwards)
}

# functions for managing the store

initStore <- function(newRowList, series) {
  # Initialize ohlcvStore as before
  ohlcvStore <- list()
  for (s in series) {
    ohlcvStore[[s]] <- xts(matrix(numeric(0), ncol = 5, dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
                           order.by = as.Date(character()))
  }
  
  # Initialize count vector with zeros for each series
  count <- rep(0, 10) # used for initializing vectors
  
  # Return the complete store
  return(list(iter = 0, ohlcv = ohlcvStore, count = count))
}


updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  # Append new data for each series specified
  for (s in series) {
    if (!is.null(newRowList[[s]])) {
      store$ohlcv[[s]] <- rbind(store$ohlcv[[s]], newRowList[[s]])
    }
  }
  
  return(store)
}
