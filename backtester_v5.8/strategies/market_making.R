# MARKET MAKING STRATEGY
# This strategy is used when the market is stable (non volatile)

source("Indicators/average_true_range.R")
getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero <- rep(0,length(newRowList)) 
  return(list(store=store,marketOrders=allzero,
              limitOrders1=allzero, 
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}
