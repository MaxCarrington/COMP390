source('./framework/data.R'); 
source('./framework/backtester.R')
source('./framework/processResults.R'); 
source('./framework/utilities.R'); # for backtestAndPlot function
source('./example_strategies.R');
source('./RiskManagement/position_size_calc.R')
source('./Plot/plot_volatility.R')
source('./Plot/plot_prices.R')
source('./StratsDataAnalysis/master_analysis.R')
source('./Plot/vector_plot.R')
source('./set_up_strategies.R')
# load data
dataList <- getData(directory="PART1")
# strategy will be passed in as a command line argument from jenkins
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  tradingStrategy <- "mean_reversion"
} else{
  tradingStrategy <- args[1]
}
# check that the choice is valid
is_valid_example_strategy <- function(tradingStrategy) { 
  tradingStrategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(tradingStrategy))


# split data in two (e.g. for in/out test)
numDays <- nrow(dataList[[1]])
inSampDays <- 560

# in-sample period
inSampleDataList <- lapply(dataList, function(x) x[1:inSampDays])

# out-of-sample period
outSampledataList <- lapply(dataList, function(x) 
  x[(inSampDays+1):numDays])

#ADD BACK IN TO TEST ON ALL STRATEGIES
strategies <- suitableStratslist(inSampleDataList)
params <- setUpTradingParams(tradingStrategy, strategies)

#params <- list(series = c(4), lookback = c(12), holdingPeriod = c(12),rsiLookback = 30, emaLookback = 30, 
               #smaLookback = 30, wmaLookback = 30, maThreshold = 0.7, overboughtThresh = 60, oversoldThresh = 40, maType = "SMA")

load_strategy(tradingStrategy, params) # function from example_strategies.R

sMult <- 0.20 # slippage multiplier
results <- backtest(inSampleDataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')


for (i in 1:length(results$pnlList)) {
  cat("Time Series", i,":","\n")
  cat("Final Cumulative PD ratio:", tail(results$pnlList[[i]]$CumPnL, 1), "\n")
}
cat("Final Account Balance:", last(results$netWorthList), "\n")
