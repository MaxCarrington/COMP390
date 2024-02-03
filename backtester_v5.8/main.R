source('./framework/data.R'); 
source('./framework/backtester.R')
source('./framework/processResults.R'); 
source('./framework/utilities.R'); # for backtestAndPlot function
source('./example_strategies.R');
source('./risk_management/position_size_calc.R')
source('./plotting/plot_code/plot_volatility.R')
source('./plotting/plot_code/plot_prices.R')
source('./strats_data_analysis/master_analysis.R')
source('./plotting/plot_code/vector_plot.R')
# load data
dataList <- getData(directory="PART1")
# strategy will be passed in as a command line argument from jenkins
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  strategy <- "bbands_contrarian"
} else{
  strategy <- args[1]
}
# check that the choice is valid
is_valid_example_strategy <- function(strategy) { 
    strategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(strategy))

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R

# split data in two (e.g. for in/out test)
numDays <- nrow(dataList[[1]])
inSampDays <- 560

# in-sample period
inSampleDataList <- lapply(dataList, function(x) x[1:inSampDays])

# out-of-sample period
outSampledataList <- lapply(dataList, function(x) 
  x[(inSampDays+1):numDays])
#Stuff for the Kelly formula
#trades = list(
  #wins = c(10, 20, 30, 10, 20, 20, 30),
  #losses = c(-10, -20, -10)
#)
#analysePreviousTrades(trades)
#position_size = calculatePositionSize()
#print(position_size)

# Assuming 'inSampleDataList' is a list of data frames where each data frame represents a series
# and 'vixLookback' is a list or other structure with predefined lookback periods
lookbackSizes <- list(weekly = 7, 
                     fortnightly =14, 
                     monthly = 30,
                     allInSampDays = inSampDays
)


# Calculates the VIX for every week over the in-sample data, for each time series
weeklyATRs <- lapply(inSampleDataList, function(series) {
  calculateATRForRangeXTS(series, lookbackSizes$weekly)
})

weeklyVIXs <- lapply(inSampleDataList, function(series) {
  calculateVIXForRangeXTS(series, lookbackSizes$weekly)
})
strategies <- list(meanReversion = "Mean-Reversion",
                   momentum ="Momentum",
                   marketMaking = "Market-Making")
index <- 2
lookback <- 7
volThresh <- 0.85
volWSize <- 30
pValueThreshMR <- 0.95
momentumWSize <- 30
pValueThreshMom <- 0.95

momentumLenThresh <- 0.90
seriesAnalysisInfo <- list()

# Initialize an empty list to hold the analysis and strategy for each series
seriesAnalysisInfo <- list()

# Loop through each series in the dataset
for(i in 1:length(inSampleDataList)) {
  series <- inSampleDataList[[i]]
  
  # Analyze the series for various factors
  volatilityStats <- analyseVolatility(series, lookback)
  volumeStats <- analyseVolume(series, lookback, volWSize, volThresh)
  momentumStats <- analyseMomentum(series, momentumWSize, pValueThreshMom, momentumLenThresh)
  mrStats <- analyseMR(series, i, pValueThreshMR)
  
  # Determine the strategy based on the analysis
  strategy <- "None" # Default strategy
  if(mrStats$meanRevScore >= 20 && momentumStats$stratType == strategies$meanReversion) {
    strategy <- strategies$meanReversion
  } else if(momentumStats$stratType == strategies$momentum) {
    strategy <- strategies$momentum
  } else if(volatilityStats$seriesVol == "Non-Volatile") {
    strategy <- strategies$marketMaking
  }
  # Append the analysis and determined strategy to the list
  seriesAnalysisInfo[[length(seriesAnalysisInfo) + 1]] <- list(
    volatility = volatilityStats,
    volume = volumeStats,
    momentum = momentumStats,
    meanReversion = mrStats,
    index = i,
    strategy = strategy # Include the strategy directly
  )
}



#seriesCharacteristics <- lapply(inSampleDataList, function(series){
  #analyseDataForStrategies(series, index, lookback)
  #index <- index + 1
#})



# Now, you can further analyze or plot these liquidity indicators alongside your volume analysis.

































#sMult <- 0.20 # slippage multiplier
#results <- backtest(dataList,getOrders,params,sMult)
#pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')

#for (i in 1:length(results$pnlList)) {
  #cat("Time Series", i,":","\n")
  #cat("Final Cumulative PnL:", tail(results$pnlList[[i]]$CumPnL, 1), "\n")
#}

#Print the final account balance
#final_balance <- pfolioPnL$pfoliosPnL$CumPnL[nrow(pfolioPnL$pfoliosPnL)]
#cat("Portfolio Profit and Loss: ", round(final_balance, digits=2), "\n")
