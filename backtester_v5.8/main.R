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

strategies <- list(meanReversion = "Mean-Reversion",
                   momentum ="Momentum",
                   marketMaking = "Market-Making")

meanRevSeriesIndexes <- c(); momentumSeriesIndexes <- c(); marketMakingSeriesIndexes <-c()

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
  if(momentumStats$stratType == strategies$meanReversion){
    mrStats$meanRevScore <- mrStats$meanRevScore + 5
  }
  # Determine the strategy based on the analysis
  strategy <- "None" # Default strategy
  if(mrStats$meanRevScore >= 25){
    meanRevSeriesIndexes <- c(meanRevSeriesIndexes, i)
    halfLives <- c(halfLives, mrStats$attributes$HalfLife$HalfLife_WithIntercept)
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

lookbackSizes <- list(weekly = 7, 
                     fortnightly =14, 
                     monthly = 30,
                     allInSampDays = inSampDays
)
print(meanRevSeriesIndexes)
print(halfLives)
#Add check in
if(tradingStrategy == "mean_reversion") {
  if(length(meanRevSeriesIndexes) > 0){
    example_params[["mean_reversion"]][["series"]] <- meanRevSeriesIndexes
    example_params[["mean_reversion"]][["halfLives"]] <- halfLives
  }
  else{
    cat("Mean reverison strategy can not run. No series are deemed suitable.", "\n")
  }
  
}


load_strategy(tradingStrategy) # function from example_strategies.R

sMult <- 0.20 # slippage multiplier
results <- backtest(inSampleDataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')


for (i in 1:length(results$pnlList)) {
  cat("Time Series", i,":","\n")
  cat("Final Cumulative PD ratio:", tail(results$pnlList[[i]]$CumPnL, 1), "\n")
}
cat("Final Account Balance:", last(results$netWorthList), "\n")

#Print the final account balance
#final_balance <- pfolioPnL$pfoliosPnL$CumPnL[nrow(pfolioPnL$pfoliosPnL)]
#cat("Portfolio Profit and Loss: ", round(final_balance, digits=2), "\n")
