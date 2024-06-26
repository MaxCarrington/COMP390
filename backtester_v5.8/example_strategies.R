source('./Indicators/half_life_of_mean_reversion.R')
example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi_contrarian", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "extreme_limit",
                        "market_making",
                        "momentum",
                        "mean_reversion"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "big_spender"=list(sizes=rep(1,10)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "rsi_contrarian"=list(lookback=10,threshold=25,series=1:5),
                    "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1:4,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=6),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "market_making"=list(series = c(5),lookback = c(20), liquidityThresh = 0.95, windowSize =30, highLiquidityPrdsThresh = 10, volatilityLookback = 10),
                    "momentum"=list(series = c(), lookback = c(), holdingPeriod = c(),rsiLookback = 30, emaLookback = 30, 
                                    smaLookback = 30, wmaLookback = 30, maThreshold = 0.7, overboughtThresh = 60, oversoldThresh = 40, maType = "SMA"),
                    "mean_reversion"=list(stdDev=2, series = c(), halfLives = c(), posSizes=rep(1,10))
                    )

load_strategy <- function(strategy, parameters) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- parameters
    print("Parameters:")
    print(params)
}
