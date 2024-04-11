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
#Potentially implement
priceChangeSinceHalfLife <- function(){
  
}