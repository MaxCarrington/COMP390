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
  tradingStrategy <- "momentum"
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
  if(momentumStats$stratType == strategies$momentum) {
    strategy <- strategies$momentum
  }else if(mrStats$meanRevScore >= 40){
    strategy <- strategies$meanReversion
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


# Extract Mean-Reversion Series Indexes
meanRevSeriesIndexes <- sapply(seriesAnalysisInfo, function(x){
  if(x$strategy == "Mean-Reversion")
    x$index
  else
    NA
})
meanRevSeriesIndexes <- na.omit(meanRevSeriesIndexes)
attr(meanRevSeriesIndexes, "na.action") <- NULL

# Get the corresponding half life for each mean reverting time series
halfLives <- sapply(seriesAnalysisInfo, function(x){
  if(x$strategy == "Mean-Reversion")
    x$meanReversion$attributes$HalfLife$HalfLife_WithIntercept
  else
    NA
})
halfLives <- na.omit(halfLives)
attr(halfLives, "na.action") <- NULL
# Extract Momentum Series Indexes
momentumSeriesIndexes <- sapply(seriesAnalysisInfo, function(x){
  if(x$strategy == "Momentum"){
    x$index
  }
  else
    NA
})
momentumSeriesIndexes <- na.omit(momentumSeriesIndexes)
attr(momentumSeriesIndexes, "na.action") <- NULL

# Calculate optimal lookback periods for Momentum strategy
momentumLookbacks <- lapply(seriesAnalysisInfo, function(x) {
  if (x$strategy == "Momentum") {
    return(x$momentum$seriesMomentum$optimalLookbackPeriod)
  } else {
    return(NULL)  # Return NULL for non-Momentum strategies
  }
})

# Remove NULL values from the list
momentumLookbacks <- momentumLookbacks[!sapply(momentumLookbacks, is.null)]

# Calculate optimal holding periods for Momentum strategy
momentumHoldings <- lapply(seriesAnalysisInfo, function(x) {
  if (x$strategy == "Momentum") {
    return(x$momentum$seriesMomentum$optimalHoldingPeriod)
  } else {
    return(NULL)  # Return NULL for non-Momentum strategies
  }
})

# Remove NULL values from the list
momentumHoldings <- momentumHoldings[!sapply(momentumHoldings, is.null)]
# Extract Market-Making Series Indexes
marketMakingSeriesIndexes <- sapply(seriesAnalysisInfo, function(x){
  if(x$strategy == "Market-Making")
    x$index
  else
    NA
})
marketMakingSeriesIndexes <- na.omit(marketMakingSeriesIndexes)
attr(marketMakingSeriesIndexes, "na.action") <- NULL
#Add check in
if(tradingStrategy == "mean_reversion") {
  if(length(meanRevSeriesIndexes) > 0){
    example_params[["mean_reversion"]][["series"]] <- meanRevSeriesIndexes
    example_params[["mean_reversion"]][["halfLives"]] <- halfLives
  }
  else{
    cat("Mean reverison strategy can not run. No series are deemed suitable.", "\n")
  }
}else if(tradingStrategy == "momentum") {
  if(length(momentumSeriesIndexes) > 0){
    example_params[["momentum"]][["series"]] <- momentumSeriesIndexes
    example_params[["momentum"]][["lookback"]] <- momentumLookbacks
    example_params[["momentum"]][["holdingPeriod"]] <- momentumHoldings
  }
  else{
    cat("Momentum strategy can not run. No series are deemed suitable.", "\n")
  }
}else if(tradingStrategy == "market_making"){
  if(length(momentumSeriesIndexes) > 0){
    example_params[["market_making"]][["series"]] <- momentumSeriesIndexes
  }
  else{
    cat("Momentum strategy can not run. No series are deemed suitable.", "\n")
  }
}

load_strategy(tradingStrategy) # function from example_strategies.R

sMult <- 0.20 # slippage multiplier
results <- backtest(inSampleDataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')


#for (i in 1:length(results$pnlList)) {
  #cat("Time Series", i,":","\n")
  #cat("Final Cumulative PD ratio:", tail(results$pnlList[[i]]$CumPnL, 1), "\n")
#}
#cat("Final Account Balance:", last(results$netWorthList), "\n")
