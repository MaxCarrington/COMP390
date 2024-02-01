source('./framework/data.R'); 
source('./framework/backtester.R')
source('./framework/processResults.R'); 
source('./framework/utilities.R'); # for backtestAndPlot function
source('./example_strategies.R');
source('./risk_management/position_size_calc.R')
source('./plotting/plot_code/plot_volatility.R')
source('./plotting/plot_code/plot_prices.R')
source('./strats_data_analysis/mean_reverting_analysis.R')
source('./strats_data_analysis/volatility_analysis.R')
source('./strats_data_analysis/momentum_analysis.R')
source('./plotting/plot_code/vector_plot.R')
source('./strats_data_analysis/volume_analysis.R')
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
series <- inSampleDataList[[1]]
windowSize <- 30
threshold <- 0.85
volIncWithTrend(series, windowSize)

highLiq <- lapply(inSampleDataList, function(x){
  correlation <- combinedLiquidityAnalysis(x, windowSize, threshold, windowSize)
})



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
