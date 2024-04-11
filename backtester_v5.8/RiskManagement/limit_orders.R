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