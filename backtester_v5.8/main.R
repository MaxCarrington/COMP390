source('./framework/data.R'); 
source('./framework/backtester.R')
source('./framework/processResults.R'); 
source('./framework/utilities.R'); # for backtestAndPlot function
source('./example_strategies.R');

# load data
dataList <- getData(directory="PART1")
# strategy will be passed in as a command line argument from jenkins
args <- commandArgs(trailingOnly = TRUE)
print("Test1")
if (length(args) < 1) {
  strategy <- "fixed"
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
#numDays <- nrow(dataList[[1]])
#inSampDays <- 550

# in-sample period
#dataList <- lapply(dataList, function(x) x[1:inSampDays])

# out-of-sample period
#dataList <- lapply(dataList, function(x) 
                               #x[(inSampDays+1):numDays])

sMult <- 0.20 # slippage multiplier
results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')

for (i in 1:length(results$pnlList)) {
  cat("Time Series", i, "\n")
  cat("Final Cumulative PnL:", tail(results$pnlList[[i]]$CumPnL, 1), "\n")
}
