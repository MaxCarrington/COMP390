
# Mean reversion source file
getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  cl <- newRowList[[params$series[1]]]$Close
  halfLife <- params$halfLives[[params$series[1]]]$HalfLife_WithIntercept #68
  

  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  store <- updateStore(store, newRowList, params$series)    
  marketOrders <- -currentPos; pos <- allzero
  
  #If we have enough periods have passed.
  if (store$iter > halfLife) {
    startIndex <-  store$iter - params$lookback
    #Iterate through each suitable series.
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      hlcPrices <- store$ohlcv[[params$series[i]]][, c("High", "Low", "Close")]
      bbands <- calculateBollingerBands(hlcPrices, halfLife, params$stdDev)
      if(cl < bbands[,"dn"]){
        print("Strategy would place a buy order as OVERSOLD")
        pos[params$series[i]] <- params$posSizes[params$series[i]]
      }
      else if (cl < bbands[,"up"]){
        print("Strategy would place a sell order as OVERBOUGHT")
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
      }
      # check if we have been in trade too long
      
      # we maintain that pos[i] is an integer
      # if pos[i] == 0 we were flat last period
      # if pos[i] >  0 we have been long  for store$count[i] periods
      # if pos[i] <  0 we have been short for store$count[i] periods
      
      if (pos[params$series[i]] == 1) {# long signal today 
        if (store$count[i] < 0) # last time we were short
          store$count[i] == pos[params$series[i]] # == 1
        else if (store$count[i] == halfLife) { # reached holding period
          pos[params$series[i]] <- 0 # don't stay long
          store$count[i] <- 0 # reset count to 0
        }
        else # 0 <= store$count[i] != (should be <) params$holdPeriod
          store$count[i] <- store$count[i] + 1 
      }
      
      else if (pos[params$series[i]] == -1) {# short signal today
        if (store$count[i] > 0) # last time we were long
          store$count[i] == pos[params$series[i]] # == -1
        else if (store$count[i] == -halfLife) { # reached holding period
          pos[params$series[i]] <- 0 # don't stay short
          store$count[i] <- 0 # reset count to 0
        }
        else # 0 >= store$count[i] != (should be >) -params$holdPeriod
          store$count[i] <- store$count[i] - 1 
      }
      else
        store$count[i] <- 0 # reset count to 0
    }
    marketOrders <- marketOrders + pos
    #cat(formatC(store$count,2),'\t',formatC(pos,2),'\n')
    #cat(formatC(pos,2),'\n')
    return(list(store=store,marketOrders=marketOrders,
                limitOrders1=allzero,limitPrices1=allzero,
                limitOrders2=allzero,limitPrices2=allzero))
  }
  
  
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  
  return(list(store=store,marketOrders=allzero,
              limitOrders1=allzero, 
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}

# Function to calculate Bollinger bands based on a lookback and standard deviation
calculateBollingerBands <- function(hlcPrices, lookback, stdDev) {
  # Ensure that hlcPrices is an xts object with High, Low, and Close columns
  if (is.xts(hlcPrices) && all(c("High", "Low", "Close") %in% colnames(hlcPrices))) {
    bbands <- BBands(HLC = hlcPrices, n = lookback, sd = stdDev)
    return(bbands)
  } else {
    stop("hlcPrices must be an xts object with High, Low, and Close columns.")
  }
}


initStore <- function(newRowList, series) {
  # Initialize ohlcvStore as before
  ohlcvStore <- list()
  for (s in series) {
    ohlcvStore[[s]] <- xts(matrix(numeric(0), ncol = 5, dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
                           order.by = as.Date(character()))
  }
  
  # Initialize count vector with zeros for each series
  count <- vector(mode = "numeric", length = length(series))
  
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


