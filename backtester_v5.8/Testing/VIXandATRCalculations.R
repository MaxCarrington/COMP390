#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# LookbackSizes list, used for all
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

lookbackSize <- list(weekly = 7, 
                     fortnightly =14, 
                     monthly = 30,
                     allInSampDays = inSampDays
)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculation of Volatility indexes for each time series
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# Assuming inSampleDataList is a list of data frames, and vixLookback is a structure holding different lookback values

# Calculates the VIX for every week over the in-sample data, for each time series
weeklyVIXs <- lapply(inSampleDataList, function(series) {
  calculateVIXForRange(series, numToCheck = weeksToCheck, lookback = lookbackSize$weekly, rangeIncrease = 7)
})

# Calculates the VIX for every month over the in-sample data, for each time series
monthlyVIXs <- lapply(inSampleDataList, function(series) {
  calculateVIXForRange(series, numToCheck = monthsToCheck, lookback = lookbackSize$monthly, rangeIncrease = 30)
})

# Calculates the VIX for every fortnight over the in-sample data, for each time series
fortnightlyVIXs <- lapply(inSampleDataList, function(series) {
  calculateVIXForRange(series, numToCheck = fortnightsToCheck, lookback = lookbackSize$fortnightly, rangeIncrease = 14)
})


#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculation of Average true ranges for each time series
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the weekly ATR for each series
weeklyATRs <- lapply(inSampleDataList, function(series) {
  calculateATRForRange(series, numToCheck = weeksToCheck, lookback = lookbackSize$weekly, rangeIncrease = 7)
})


# Calculate the monthly ATR for each series
monthlyATRs <- lapply(inSampleDataList, function(series) {
  calculateATRForRange(series, numToCheck = monthsToCheck, lookback = lookbackSize$monthly, rangeIncrease = 30)
})

# Calculate the fortnightly ATR for each series
fortnightlyATRs <- lapply(inSampleDataList, function(series) {
  calculateATRForRange(series, numToCheck = fortnightsToCheck, lookback = lookbackSize$fortnightly, rangeIncrease = 14)
})


# Saving all VIX information for retrieval as computation is slow
saveRDS(weeklyVIXs, file = "./Time_series_analysis_data/PART1/weeklyVIXs.rds")
saveRDS(fortnightlyVIXs, file = "./Time_series_analysis_data/PART1/fortnightlyVIXs.rds")
saveRDS(monthlyVIXs, file = "./Time_series_analysis_data/PART1/monthlyVIXs.rds")

# Saving all ATR information for retrieval as computation is slow
saveRDS(weeklyATRs, file = "./Time_series_analysis_data/PART1/weeklyATRs.rds")
saveRDS(fortnightlyATRs, file = "./Time_series_analysis_data/PART1/fortnightlyATRs.rds")
saveRDS(monthlyATRs, file = "./Time_series_analysis_data/PART1/monthlyATRs.rds")
# Reading in ATR data from file example
fortnightlyATRs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/fortnightlyATRs.rds")
weeklyATRs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/weeklyATRs.rds")
monthlyATRs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/monthlyATRs.rds")

fortnightlyVIXs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/fortnightlyVIXs.rds")
weeklyVIXs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/weeklyVIXs.rds")
monthlyVIXs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/monthlyVIXs.rds")

plotVIXData("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/weeklyVIXs.rds", 
            "/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/monthlyVIXs.rds", 
            "/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/fortnightlyVIXs.rds", 
            titleString = "VIX Analysis")

fortnightlyATRs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/fortnightlyATRs.rds")
fortnightlyVIXs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/PART1/fortnightlyVIXs.rds")
plotATRandVIXComparison(fortnightlyATRs, fortnightlyVIXs)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------