#------------------------------------------------------------------------------------------------------
# Main strategy function that operates through each series. Based on the series' market conditions,
# volatility and liquidity, market orders are executed based on stable conditions. 
#------------------------------------------------------------------------------------------------------

source("Indicators/average_true_range.R")
source("StratsDataAnalysis/volatility_analysis.R")
source("StratsDataAnalysis/volume_analysis.R")


getOrders <- function(store, newRowList, currentPos, info, params) {
  standardOrderSize <- 10 #Change this -> dynamically change the size based on the amount of money left in the account and the price of the asset in relation to this!
  confidence <- 0.5 # Is conservative currently, change this when optimising parameters and put it as a parameter
  liquidityLookback <- 15 #Add this into params MUST BE ODD
  # Initialise the store if it has not already been initialised
  if (is.null(store))
    store <- initStore(newRowList, params$series)
  
  # Update the store with the new days data 
  store <- updateStore(store, newRowList, params)    
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  marketOrders <- -currentPos; pos <- allzero
  
  limitOrders1 <- allzero; limitPrices1 <- allzero; limitOrders2 <- allzero;limitPrices2 <- allzero
  
  # Iterate through each suitable series
  for (i in 1:length(params$series)) {
    #Ensure enough days have passed based on the lookback parameter
    if(store$iter > (params$lookback)){
      
      seriesIndex <- params$series[i]
      #Fetch the current period volatility and the series volatility from the store
      currentPeriodVol <- tail(store$periodVolatilities[[seriesIndex]], 1)
      currentSeriesVol <- tail(store$seriesVolatilities[[seriesIndex]], 1)
      
      #If the volatility of the current lookback period is low then determine the liqudity 
      if(length(currentPeriodVol) > 0 && currentPeriodVol == "Low"){
        
        #Calculate the most recent useable liquidity and spread
        highLiquidity <- tail(store$liquidity[[seriesIndex]], 1)
        spread <-tail(store$spread[[seriesIndex]], 1)
        #print(store$liquidity[[seriesIndex]])
        #If a high liquidity period is detected, change order size based on the volatility of the total series
        
        directionLiquidities <- tail(store$liquidity[[seriesIndex]], liquidityLookback)
        #If we are in a period of high Liquidity, determine the trade direction and the order size and execute if
        # determineTradeDirection does not indicate it is a bad time to trade ("hold")
        if(highLiquidity){
          direction <- determineTradeDirection(store$ohlcv[[seriesIndex]]$Close, store$ohlcv[[seriesIndex]]$Volumes, directionLiquidities, liquidityLookback)
          if(direction != "hold"){
            orderSize <- determineOrderSize(currentPos[i], currentSeriesVol, highLiquidity, standardOrderSize, confidence, direction)
            closePrice <- tail(store$ohlcv[[seriesIndex]]$Close, 1)
            if (direction == "buy") {
              limitPrices1[seriesIndex] <- closePrice - spread / 2
              limitOrders1[seriesIndex] <- orderSize
            } else if (direction == "sell") {
              limitPrices2[seriesIndex] <- closePrice + spread / 2
              limitOrders2[seriesIndex] <- orderSize
          }
          }
      }
    }
    }
  }
  # Return updated orders and store
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}

# This function determines the direction of a trade based on an SMA and the current price in relation to this
# SMA. If the price is greater then we are in an uptrend and if the price is lower, we are in a short term
# downtrend. Then look at the liqudities and if the previous liquidities are higher than the current ones, liquidity is
# increasing, otherwise it is decreasing.
determineTradeDirection <- function(prices, volumes, liquidities, lookback = 15){
  
  liquidityThresh <- 0.4
  
  if(lookback > nrow(prices))
    lookback <- nrow(prices)
    # Calculate the short-term moving average
  shortTermMA <- tail(SMA(prices, n = lookback), 1)
  # Determine the trend direction
  trendDirection <- ifelse(tail(prices, 1) > shortTermMA, "upwards", "downwards")
  
  # Calculate the proportion of high liquidity periods for early and later parts of the lookback period
  if(lookback > length(liquidities))
    lookback <- length(liquidities)
  
  splitPoint <- floor(lookback / 2)
  earlyHighLiqProportion <- mean(liquidities[1:splitPoint])
  laterHighLiqProportion <- mean(liquidities[(splitPoint + 1):lookback])
  liquidityDirection <- ifelse(laterHighLiqProportion > earlyHighLiqProportion, "increasing", "decreasing")
  
  print(paste("The trend direction is", trendDirection, "and the liquidity direction is", liquidityDirection))
  action <- "hold" # Hold if the trend and liquidity direction do not match
  if (trendDirection == "upwards" && liquidityDirection == "increasing") { #If liquidity and price is increasing, buy
    action <- "buy"
  } else if (trendDirection == "downwards" && liquidityDirection == "decreasing") { #If liquidity and price is decreasing, sell
    action <- "sell"
  }
  return(action)
}

# Function to calculate order size based on the parameters given.
determineOrderSize <- function(currentPosition, volatility, liquidity, stdOrderSize, confidence, direction) {
  volatilityAdjustment <- volAdjustment(volatility)
  liquidityAdjustment <- if (liquidity) 1.1 else 0.9
  # Used to normalise the result
  normaliser <- 10000
  
  # Initial order size calculation remains the same
  baseOrderSize <- confidence * stdOrderSize * volatilityAdjustment * liquidityAdjustment
  adjustedOrderSize <- baseOrderSize * (1 - abs(currentPosition / normaliser))
  
  # Adjust the order size further based on order type
  finalOrderSize <- switch(direction,
                           "buy" = adjustedOrderSize * 1,
                           "sell" = adjustedOrderSize * -1)
  
  return(max(min(finalOrderSize, 1000), -1000))
}


limitPriceAdjustments <- function(){
  
}

#Adjusts the order size based on the volatility of the current series
volAdjustment <- function(volatility){
  orderSizeAdjustment <- switch(volatility,
                                "Non-Volatile" = 1.5,
                                "Volatile" = 0.75,
                                "Highly Volatile" = 0.5)
  return(orderSizeAdjustment)
}

#REMOVE
isLiquidityHigh <- function(series, storeIter, volumeLookback, thresholdPercentage = 1.75) {
  highLiquidity <- FALSE
  
  # Ensure there is enough data to perform the calculation
  if(length(series) > 0 && nrow(series) >= volumeLookback) {
    # Exclude the most recent volume data point, as it may not be fully available
    useableVolumes <- head(series$Volume, storeIter - 1)
    
    # Calculate the historical average volume over the specified lookback period
    historicalAvgVolume <- mean(tail(useableVolumes, volumeLookback), na.rm = TRUE)
    
    # Define the liquidity threshold as a certain percentage above this historical average
    liquidityThreshold <- historicalAvgVolume * thresholdPercentage
    
    # Compare the average volume of the most recent period to this liquidity threshold
    # To focus on the most recent period, ensure you're comparing the right volume against the threshold
    recentVolume <- tail(useableVolumes, 1)
    highLiquidity <- recentVolume > liquidityThreshold
  }
  return(highLiquidity)
}



#Initialises the store 
initStore <- function(newRowList, series) {
  ohlcvStore <- list()
  count <- vector("numeric", length(series))
  periodVolatilities <- vector("list", length(series))
  liquidity <- vector("list", length(series))
  spreads <- vector("list", length(series))
  seriesVolatilities <- vector("list", length(series))
  for (s in series) {
    ohlcvStore[[s]] <- xts(matrix(numeric(0), ncol = 5, dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume"))),
                           order.by = as.Date(character()))
    # Initialise stores for each series to store relevant information
    periodVolatilities[[s]] <- NA
    liquidity[[s]] <- NA
    spreads[[s]] <- NA
    seriesVolatilities[[s]] <- NA
    }
  
  # Return store
  return(list(iter = 0, 
              ohlcv = ohlcvStore, 
              count = count, 
              periodVolatilities = periodVolatilities, 
              liquidity = liquidity, 
              spreads = spreads,
              seriesVolatilities = seriesVolatilities))
}


#Updates the store.
updateStore <- function(store, newRowList, params) {
  store$iter <- store$iter + 1
  
  for (i in params$series) {
    if (!is.null(newRowList[[i]])) {
      store$ohlcv[[i]] <- rbind(store$ohlcv[[i]], newRowList[[i]])
      
      #Ensures that there are enough periods for the lookback
      if(store$iter > max(params$lookback, params$volatilityLookback)){
        
        #Calculate new volatility, liquidity and spread. 
        newVols <- calculateVolatility(store$ohlcv[[i]], store$iter, params$volatilityLookback)
        newLiq <- isLiquidityHigh(store$ohlcv[[i]]$Volume, store$iter, params$lookback)
        newSpread <- calculateSpreadUsingLiquidity(store$ohlcv[[i]], store$iter, newLiq)
        
        #Ensure that the period volatilities are not null, if they are just return a list
        if (is.null(store$periodVolatilities[[i]])) {
          store$periodVolatilities[[i]] <- list()
        }
        store$periodVolatilities[[i]] <- c(store$periodVolatilities[[i]], newVols$currPeriod)
        
        #Ensure that the series volatilities are not null, if they are just return a list
        if (is.null(store$seriesVolatilities[[i]])) {
          store$seriesVolatilities[[i]] <- list()
        }
        store$seriesVolatilities[[i]] <- c(store$seriesVolatilities[[i]], newVols$seriesVol)
        
        #Update the new values the store
        store$liquidity[[i]] <- c(store$liquidity[[i]], newLiq)
        store$spread[[i]] <- c(store$spread[[i]], newSpread)
      }
    }
  }
  
  return(store)
}


calculateVolatility <- function(series, storeIter, volatilityLookback){
  
  #Ensure we are not using todays data as this would not be available
  useableSeries <- head(series, storeIter -1)
  #Calculates the volatility for all of the data so far
  volatility <- analysePeriodVol(useableSeries, volatilityLookback)
  # Returns a list with the latest volume period and the volume over the entire period so far.
  return(list(currPeriod = volatility$periodVol,
              seriesVol = volatility$seriesVol))
  
}

#Calculate the spread based on the liquidity, if liquidity is high, lower the spread, otherwise incerase the spread
calculateSpreadUsingLiquidity <- function(series, storeIter, liquidity) {
  useableSeries <- head(series, storeIter - 1)
  spread <- NA
  
  if (nrow(useableSeries) > 0) {
    todaysData <- tail(useableSeries, 1)[1,]
    baseSpread <- todaysData$High - todaysData$Low 
    spread <- if (liquidity) {
      baseSpread * 0.75
    } else {
      baseSpread * 1.25
    }
  }
  
  return(spread)
}
