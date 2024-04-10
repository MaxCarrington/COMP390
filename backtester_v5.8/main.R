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
#mean_reversion = list(stdDev=2, series = c(6,7,8), halfLives = c(48, 42, 50),tradeHistory = list(wins = numeric(), losses = numeric()))
#market_making=list(series = c(5),lookback = c(20), liquidityThresh = 0.95, windowSize =30, highLiquidityPrdsThresh = 10, volatilityLookback = 10, tradeHistory = list(wins = numeric(), losses = numeric()))
#momentum=list(series = c(4), lookback = c(12), holdingPeriod = c(12),rsiLookback = c(30), emaLookback = c(30), smaLookback = c(30), wmaLookback = c(30), maThreshold = 0.7, overboughtThresh = 60, oversoldThresh = 40, maType = "SMA",tradeHistory = list(wins = numeric(), losses = numeric()))
args <- commandArgs(trailingOnly = TRUE)
params <- list()
if (length(args) < 1) {
  tradingStrategy <- "mean_reversion"
  params <- list(stdDev=2, series = c(6,7,8), halfLives = c(48, 42, 50),tradeHistory = list(wins = numeric(), losses = numeric()))
} else{
 tradingStrategy <- args[1]
 if(args[1] == "mean_reversion")
   params <- list(stdDev=2, series = c(6,7,8), halfLives = c(48, 42, 50),tradeHistory = list(wins = numeric(), losses = numeric()))
 else if(args[1] == "momentum")
   params <-list(series = c(4), lookback = c(12), holdingPeriod = c(12),rsiLookback = c(30), emaLookback = c(30), smaLookback = c(30), wmaLookback = c(30), maThreshold = 0.7, overboughtThresh = 60, oversoldThresh = 40, maType = "SMA",tradeHistory = list(wins = numeric(), losses = numeric()))
 else if(args[1] == "market_making")
   params <- list(series = c(5),lookback = c(20), liquidityThresh = 0.95, windowSize =30, highLiquidityPrdsThresh = 10, volatilityLookback = 10, tradeHistory = list(wins = numeric(), losses = numeric()))
 
}

# check that the choice is valid
is_valid_example_strategy <- function(tradingStrategy) { 
  tradingStrategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(tradingStrategy))


# split data in two (e.g. for in/out test)
numDays <- nrow(dataList[[1]])
inSampDays <- 560
#inSampDays <- 560

# in-sample period
inSampleDataList <- lapply(dataList, function(x) x[1:inSampDays])
# out-of-sample period
outSampledataList <- lapply(dataList, function(x) 
  x[(inSampDays+1):numDays])


#ADD BACK IN TO TEST ON ALL STRATEGIES
#strategies <- suitableStratslist(inSampleDataList)
#params <- setUpTradingParams(tradingStrategy, strategies)


load_strategy(tradingStrategy, params) # function from example_strategies.R

sMult <- 0.20 # slippage multiplier
results <- backtest(outSampledataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')

for (i in 1:length(results$pnlList)) {
  cat("Time Series:", i, "\n")
  cat("Final Cumulative PnL:", tail(results$pnlList[[i]]$CumPnL, 1), "\n")
}

#Print the final account balance
final_balance <- pfolioPnL$pfoliosPnL$CumPnL[nrow(pfolioPnL$pfoliosPnL)]
cat("Portfolio Profit and Loss: ", round(final_balance, digits=2), "\n")
