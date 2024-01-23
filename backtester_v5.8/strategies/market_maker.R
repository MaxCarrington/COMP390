# MARKET MAKING STRATEGY
#
# This strategy is used when the market is stable (non volatile)
# This is determined by finding the Average True Range (ATR) of a time series and a calculated volatility index. 
# If the average true range 
#
source("Indicators/average_true_range.R")
getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  print(currentPos)
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
