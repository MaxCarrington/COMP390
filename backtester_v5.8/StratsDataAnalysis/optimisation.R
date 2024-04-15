# Load necessary library
if (!require("GA")) {
  install.packages("GA")
}
library(GA)

# Define the fitness function
fitnessFunction <- function(params) {
  # Unpack parameters
  lookback <- as.integer(params[1])
  holdingPeriod <- as.integer(params[2])
  rsiLookback <- as.integer(params[3])
  emaLookback <- as.integer(params[4])
  smaLookback <- as.integer(params[5])
  wmaLookback <- as.integer(params[6])
  maThreshold <- params[7]
  overboughtThresh <- params[8]
  oversoldThresh <- params[9]
  
  # Set up dynamic parameters
  dynamic_params <- list(
    series = c(4), 
    lookback = lookback, 
    holdingPeriod = holdingPeriod,
    rsiLookback = rsiLookback, 
    emaLookback = emaLookback, 
    smaLookback = smaLookback, 
    wmaLookback = wmaLookback, 
    maThreshold = maThreshold, 
    overboughtThresh = overboughtThresh, 
    oversoldThresh = oversoldThresh,
    maType = "WMA"
  )
  
  # Load strategy with updated parameters
  load_strategy('momentum', dynamic_params)
  
  # Execute backtest
  results <- backtest(inSampleDataList, getOrders, dynamic_params, sMult)
  pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')
  returns <- diff(log(inSampleDataList[[1]]$Close))  # Make sure to adjust this index according to your data structure
  negative_sharpe <- -calculateSharpeRatio(returns)
  return(negative_sharpe)
}

optimiseParams <- function(){
  # Set up GA
  ga <- ga(
    type = "real-valued", 
    fitness = fitnessFunction, 
    lower = c(7, 7, 7, 7, 7, 7, 0.5, 55, 15),  # Lower bounds for parameters
    upper = c(30, 30, 50, 50, 50, 50, 1.0, 85, 55),  # Upper bounds for parameters
    popSize = 50,  # Population size
    maxiter = 100  # Maximum iterations
  )
  
  # Print the best set of parameters found
  print(ga@solution)
}
# Sharpe Ratio Function
# params:
#   returns: Numeric vector of investment returns
#   riskFreeRate: Risk-free rate, annualized (default to 0 if not provided)
#   tradingDays: Number of trading days in a year (default is 252 for stock markets)

calculateSharpeRatio <- function(returns, riskFreeRate = 0, tradingDays = 252) {
  # Annualize the average return
  averageReturn <- mean(returns) * tradingDays
  
  # Annualize the standard deviation of the returns
  sdReturns <- sd(returns) * sqrt(tradingDays)
  
  # Calculate the Sharpe Ratio
  sharpeRatio <- (averageReturn - riskFreeRate) / sdReturns
  
  return(sharpeRatio)
}
