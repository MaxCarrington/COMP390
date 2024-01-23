source("framework/data.R")
source("Indicators/average_true_range.R")

dataList <- getData(directory="PART1")

d <- dataList[[1]][1:500,]
prices <- c()
lookbackSize <- 14
upperBoundPointer <- 0
lookbackCounter <- 0
lowerBoundPointer <- 1
atrIndictor <- c()
for(i in 1:501){
  if(lookbackCounter == lookbackSize){
    prices <- d[lowerBoundPointer:upperBoundPointer]
    atrIndictor <- c(atrIndictor, (calculateTrueRange(prices, lookbackSize)))
    lowerBoundPointer <- upperBoundPointer
    lookbackCounter <- 0
  }
  lookbackCounter <- lookbackCounter+1
  upperBoundPointer <- upperBoundPointer+1
}
atrDiff = diff(atrIndictor)
for(i in 1:length(atrDiff)){
  temp = atrDiff[i]
  atrDiff[i] <- abs(temp)
}
plot(atrDiff)

