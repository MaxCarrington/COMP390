#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculation of Volatility indexes for each time series
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
vixLookback <- list(weekly = 7, 
                    fortnightly =14, 
                    monthly = 30,
                    allInSampDays = inSampDays
)
#The volatility index for all of the in-sample days
allInSampVIXs <- lapply(inSampleDataList, function(series){
  inSampleTotalVIX <- annualisation(series, vixLookback$allInSampDays)
  return(inSampleTotalVIX)
})

# Number of weeks to check can be changed
weeksToCheck <- 78 # currently set to all of the in-sample weeks (in-sample / 7)
weekLength <- 0 # Set to 0 as the start range for the first week should be 1

#Calculates the VIX for every month over the in-sample data, for each time series
weeklyVIXs <- lapply(inSampleDataList, function(series){
  vixValues <- calculateVIXForRange(series=series, numToCheck = weeksToCheck, lookback = vixLookback$weekly, rangeIncrease = 7)
  return(vixValues)
})

# Number of weeks to check can be changed
monthsToCheck <- 18 # currently set to all of the in-sample months (inSample / 30 - approximated for simplicity) 
monthLength <- 0 # Set to 0 as the start range for the first week should be 1

#Calculates the VIX for every month over the in-sample data, for each time series
monthlyVIXs <- lapply(inSampleDataList, function(series){
  vixValues <- calculateVIXForRange(series=series, numToCheck = monthsToCheck, lookback = vixLookback$monthly, rangeIncrease = 30)
  return(vixValues)
})

fortnightsToCheck <- 39 # currently set to all of the in-sample fortnights (InSample / 30 - approximated for simplicity) 
fortnightLength <- 0 # Set to 0 as the start range for the first week should be 1
fortnightlyVIXs <- lapply(inSampleDataList, function(series){
  vixValues <- calculateVIXForRange(series=series, numToCheck = fortnightsToCheck, lookback = vixLookback$fortnightly, rangeIncrease = 14)
  return(vixValues)
})
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculation of Average true ranges for each time series
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#The volatility index for all of the in-sample days
allInSampATRs <- lapply(inSampleDataList, function(series){
  inSampleATR <- calculateTrueRange(series, vixLookback$allInSampDays)
  return(inSampleATR)
})

# Number of weeks to check can be changed
weeksToCheck <- 78 # currently set to all of the in-sample weeks (in-sample / 7)
weekLength <- 0 # Set to 0 as the start range for the first week should be 1

#Calculates the ATR for every week over the in-sample data, for each time series
weeklyATRs <- lapply(inSampleDataList, function(series){
  atrValues <- calculateATRForRange(series=series, numToCheck = weeksToCheck, lookback = vixLookback$weekly, rangeIncrease = 7)
  return(atrValues)
})
print(weeklyATRs)

# Number of weeks to check can be changed
monthsToCheck <- 18 # currently set to all of the in-sample months (inSample / 30 - approximated for simplicity) 
monthLength <- 0 # Set to 0 as the start range for the first week should be 1

#Calculates the ATR for every month over the in-sample data, for each time series
monthlyATRs <- lapply(inSampleDataList, function(series){
  atrValues <- calculateATRForRange(series=series, numToCheck = monthsToCheck, lookback = vixLookback$monthly, rangeIncrease = 30)
  return(atrValues)
})

#Calculates the ATR for every week over the in-sample data, for each time series
fortnightsToCheck <- 39 # currently set to all of the in-sample fortnights (InSample / 14 - approximated for simplicity) 
fortnightLength <- 0 # Set to 0 as the start range for the first week should be 1
fortnightlyATRs <- lapply(inSampleDataList, function(series){
  atrValues <- calculateATRForRange(series=series, numToCheck = fortnightsToCheck, lookback = vixLookback$fortnightly, rangeIncrease = 14)
  return(atrValues)
})

# Saving all VIX information for retrieval as computation is slow
saveRDS(weeklyVIXs, file = "./Time_series_analysis_data/weeklyVIXs.rds")
saveRDS(fortnightlyVIXs, file = "./Time_series_analysis_data/fortnightlyVIXs.rds")
saveRDS(monthlyVIXs, file = "./Time_series_analysis_data/monthlyVIXs.rds")
saveRDS(allInSampVIXs, file = "./Time_series_analysis_data/allInSampVIXs.rds")

# Saving all ATR information for retrieval as computation is slow
saveRDS(weeklyATRs, file = "./Time_series_analysis_data/weeklyATRs.rds")
saveRDS(fortnightlyATRs, file = "./Time_series_analysis_data/fortnightlyATRs.rds")
saveRDS(monthlyATRs, file = "./Time_series_analysis_data/monthlyATRs.rds")
saveRDS(allInSampATRs, file = "./Time_series_analysis_data/allInSampATRs.rds")

# Loading weeklyVIXs
#weeklyVIXs <- readRDS("./weeklyVIXs.rds")
#--------------------------------------------------------------------------------------------------------------------------------------------------------------