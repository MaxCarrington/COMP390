#Returns the take profit price for a given asset
calculateTakeProfit <- function(tradeType, entryPrice){
  #priceChange <- priceChangeSinceHalfLife  MAYBE IMPLEMENT THIS LATER
  if(tradeType == "buy"){
    exitPrice <- entryPrice * 1.25
  }else{ #Must be a sell
    exitPrice <- entryPrice * 0.75
  }
  return(coredata(exitPrice))
}
#This function checks if take profits have been hit, if they have, add them to 
# a position to take profit
checkTakeProfits <- function(store, todaysOpen, seriesIndex){
  positionSize <- 0
  tradeRecords <- store$tradeRecords[[seriesIndex]]
  latestDate <- index(last(store$ohlcv[[seriesIndex]]))
  for(i in 1:length(tradeRecords)){
    tradeRecord <- tradeRecords[[i]]
    tradeEntryPrice <- tradeRecord$entryPrice
    takeProfit <- tradeRecord$takeProfit
    orderType <- tradeRecord$tradeType
    if(orderType == "buy" && todaysOpen >= takeProfit){
      positionSize <- positionSize  - tradeRecord$positionSize
      print(paste("A Take profit has been hit, at price", todaysOpen, " and selling", positionSize, "units, to cancel long trade"))
      
    } else if(orderType == "sell" && todaysOpen <= takeProfit){
      positionSize <- positionSize + tradeRecord$positionSize
      print(paste("A Take profit has been hit, at price", todaysOpen, " and buying", positionSize, "units, to cancel short trade"))
    }
    if(positionSize != 0)
      store <- closeTradeRecord(store, seriesIndex, tradeRecord)
  }
  return(list(positionSize = positionSize, store = store))
}
#Potentially implement
priceChangeSinceHalfLife <- function(){
  
}