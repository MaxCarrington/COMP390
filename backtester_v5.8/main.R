source('./framework/data.R'); 
source('./framework/backtester.R')
source('./framework/processResults.R'); 
source('./framework/utilities.R'); # for backtestAndPlot function
source('./example_strategies.R');
source('./Indicators/average_true_range.R')
source('./Indicators/volatility_index.R')
source('./Indicators/ADF_test.R')
source('./Indicators/hurst_exponent.R')
source('./Indicators/half_life_of_mean_reversion.R')
source('./Indicators/variance_ratio_test.R')
source('./risk_management/position_size_calc.R')
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
inSampDays <- 550

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

# Reading in ATR data from file example
#allInSampATRs <- readRDS("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/Time_series_analysis_data/allInSampATRs.rds")

sMult <- 0.20 # slippage multiplier
results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')

for (i in 1:length(results$pnlList)) {
  cat("Time Series", i,":","\n")
  cat("Final Cumulative PnL:", tail(results$pnlList[[i]]$CumPnL, 1), "\n")
}

#Print the final account balance
final_balance <- pfolioPnL$pfoliosPnL$CumPnL[nrow(pfolioPnL$pfoliosPnL)]
cat("Portfolio Profit and Loss: ", round(final_balance, digits=2), "\n")
