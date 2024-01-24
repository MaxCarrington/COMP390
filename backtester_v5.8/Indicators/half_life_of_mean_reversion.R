#-------------------------------------------------------------------------------
# Calculation of the half life of mean reversion for a given time series
# The half life of mean reversion is used to determine the time it takes for an
# asset's price to return halfway to its historical average after deviating from
# it. If the half-life is low, the asset mean reverts frequently and the mean 
# reversion does not take a long time. Otherwise, if the half-life were long, 
# this would render the strategy less useful as it cannot profit regularly.
#
# Acceptable values:
# Short Half-life: 1-10 days
# Medium hald life: 10-30 days
# Long hald life: 30-60 days
#-------------------------------------------------------------------------------

calculateHalfLife <- function(timeSeries) {
  logPrices <- log(timeSeries)
  laggedLogPrices <- lag(logPrices, -1)
  diffLogPrices <- diff(logPrices)
  
  # Autoregressive model fitted to the time series
  # uses 'lm()', which is used to fit linear models.
  model <- lm(diffLogPrices ~ lagLogPrices[-1])
  theta <- coef(model)[2]
  
  # Half-life = -log(2)/log(0)
  # Calculates the half-life
  halfLife <- -log(2) / log(theta)
  print(halfLife)
  return(halfLife)
}
#-------------------------------------------------------------------------------
