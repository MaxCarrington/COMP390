#-------------------------------------------------------------------------------
# Mean-reversion strategy that uses Bollinger Bands, trades will be executed when t
# the price crosses the Bollinger Bands thresholds, expecting a reversal towards the mean.
# The key importance of this strategy is in the data analysis. The data analysis determines
# half lives and then this is used to determine holding periods for mean reverison.
#-------------------------------------------------------------------------------
# Mean reversion source file
getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  

  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  store <- updateStore(store, newRowList, params$series)    
  marketOrders <- -currentPos; pos <- allzero
  
  #Iterate through each suitable series.
  for (i in 1:length(params$series)) {
    seriesIndex <- params$series[i]
    halfLife <- params$halfLives[i]
    #If enough periods have passed.
    if (store$iter > halfLife) {
      
      #Fetch the high low and close values so fat and the half life and determine bollinger bands.
      halfLife <- params$halfLives[i]
      cl <- newRowList[[params$series[i]]]$Close
      hlcPrices <- store$ohlcv[[params$series[i]]][, c("High", "Low", "Close")]
      bbands <- calculateBollingerBands(hlcPrices, halfLife, params$stdDev)
      #Buy if the bands suggest the close is below the bollinger band
      if(cl < bbands[,"dn"]){
        pos[params$series[i]] <- params$posSizes[params$series[i]]
      }
      else if (cl < bbands[,"up"]){ #sell if the bands suggest the close is above the bollinger band
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
      }
      
      #Manages the trades duration based on the life, if a trade is open then adjust 
      # the duration of the based on how long a trade has been held.
      if (pos[params$series[i]] == 1) {
        if (store$count[i] < 0)
          store$count[i] = pos[params$series[i]] 
        else if (store$count[i] == halfLife) {
          pos[params$series[i]] <- 0
          store$count[i] <- 0
        }
        else
          store$count[i] <- store$count[i] + 1 
      }
      # does the same as above, but for sell orders
      else if (pos[params$series[i]] == -1) {
        if (store$count[i] > 0)
          store$count[i] = pos[params$series[i]]
        else if (store$count[i] == -halfLife) {
          pos[params$series[i]] <- 0
          store$count[i] <- 0
        }
        else
          store$count[i] <- store$count[i] - 1 
      }
      else
        store$count[i] <- 0
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
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
  for (s in series) {
    ohlcvStore[[s]] <- xts(matrix(numeric(0), ncol = 5, dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
                           order.by = as.Date(character()))
  }
  count <- vector(mode = "numeric", length = length(series))
  
  return(list(iter = 0, ohlcv = ohlcvStore, count = count))
}


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