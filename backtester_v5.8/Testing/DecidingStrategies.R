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