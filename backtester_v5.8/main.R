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
#Used for testing purposes ONLY-------------------------------------------------
strategies <- list(meanReversion = "mean_reversion",
                   momentum = "momentum",
                   marketMaking = "market_making")
getParams <- function(tradingStrategy){
  params <- list()
  if(tradingStrategy == strategies$meanReversion)
    params <- list(stdDev=2, 
                   series = c(6,7,8), 
                   halfLives = c(48, 42, 50),
                   pValueThreshMR = 0.95,
                   mrScoreThresh = 40
    )
  else if(tradingStrategy == strategies$momentum)
    params <- list(series = c(4), 
                   lookback = 12, 
                   holdingPeriod = 12,
                   rsiLookback = 30, 
                   emaLookback = 30, 
                   smaLookback = 30, 
                   wmaLookback = 30, 
                   maThreshold = 0.7, 
                   overboughtThresh = 60, 
                   oversoldThresh = 40, 
                   maType = "WMA",
                   momentumWSize = 30, 
                   pValueThreshMom = 0.95, 
                   momentumLenThresh = 0.90
    )
  else if(tradingStrategy == strategies$marketMaking)
    params <- list(series = c(5),
                   lookback = c(20),
                   liquidityThresh = 0.6,
                   windowSize =30,
                   highLiquidityPrdsThresh = 10,
                   volatilityLookback = 10,
                   volumeLookback = 10,
                   initialConfidence = 0.5,
                   liquidityLookback = 15,
                   priceLookback = 10,
                   volumeMultiplier = 1.1, 
                   rangeMultiplier = 1.1
    )
  return(params)
}

params <- list()
if (length(args) < 1) {
  tradingStrategy <- strategies$meanReversion
  params <- getParams(tradingStrategy)
} else{
 tradingStrategy <- args[1]
 params <- getParams(tradingStrategy)
}
#-------------------------------------------------------------------------------
# check that the choice is valid
is_valid_example_strategy <- function(tradingStrategy) { 
  tradingStrategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(tradingStrategy))

# split data in two (e.g. for in/out test)s
numDays <- nrow(dataList[[1]])
inSampDays <- 560
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

