source("framework/data.R")
source("Indicators/volatility_index.R")


dataList <- getData(directory="PART1")

d <- dataList[[1]][1:500,]
lookbackSize <- 14
upperBoundPointer <- 0
lookbackCounter <- 0
lowerBoundPointer <- 1
vixIndicator <- c()
annualised <- c()
for(i in 1:501){
  if(lookbackCounter == lookbackSize){
    prices <- d[lowerBoundPointer:upperBoundPointer]
    vixIndicator <- c(vixIndicator, (dailyVolatility(prices, lookbackSize)))
    annualised <- c(annualised, annualisation(dailyVolatility(prices, lookbackSize)))
    lowerBoundPointer <- upperBoundPointer
    lookbackCounter <- 0
  }
  lookbackCounter <- lookbackCounter+1
  upperBoundPointer <- upperBoundPointer+1
}
plot(vixIndicator)
plot(annualised)