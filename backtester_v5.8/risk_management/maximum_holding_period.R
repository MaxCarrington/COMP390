#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Maximum Holding Period functionality
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# Checks if the maximum holding period for a trade has been exceeded
checkMaxHoldingPeriod <- function(dateOfTrade, currentDate, maxHoldingDays) {
  maxHoldingDays <- calculateMaxHolidingDays(series, dateOfTrade, strategy)
  limitReached <- FALSE
  daysHeld <- as.integer(difftime(currentDate, dateOfTrade, units = "days"))# Calculate the number of days the trade has been held

  if (daysHeld > maxHoldingDays) # If the number of days held is greater than the allowable days for a trade to be held
    limitReached <- TRUE
  return(limitReached)
    
}
#Determines the allowable number of trading days for a given trade
# NOTE this should be used as the parameter maxHoldingDays in checkMaxHoldingPeriod
# FINISH ONCE STRATEGIES ARE IMPLEMENTED
calculateMaxHolidingDays <- function(series, dayOfTrade, currentDate, tradeType){
  #tradeType = list(short, medium, long)
  #Analyse series to determine previous volatility, trendStrength, mean reversion tendences, etc
  #Example: Short = 10, medium = 30, long = 60 -> Optimise
  #If there is more volatilty then reduce time, etc.
}


