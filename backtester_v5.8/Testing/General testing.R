#ADF test 
library(tseries)
library(pracma)
source("framework/data.R")
source("Indicators/average_true_range.R")

#If the p-value is less than 0.05 then it is stationary
#dataList <- getData(directory="PART1")
#for(i in 1:10){
#  d <- dataList[[i]][1:500,]
#  prices <- d$Close
#  print(adf.test(prices))
#}
for(i in 1:10){
  d <- dataList[[i]][1:500,]
  print(hurstexp(d))
  
}

#Hurst exponent always range between 0 and 1 and based on the value of H
#H < 0.5 : mean-reversion to the long-term mean (anti-persistent)
#H = 0.5 : random walk
#H > 0.5 : strong trend and long memory effects (persistent)
