#-------------------------------------------------------------------------------
# Calculation of the half life of mean reversion for a given time series
#-------------------------------------------------------------------------------

# The half life of mean reversion is used to determine the time it takes for an
# asset's price to return halfway to its historical average after deviating from
# it. If the half-life is low, the asset mean reverts frequently and the mean 
# reversion does not take a long time. Otherwise, if the half-life were long, 
# this would render the strategy less useful as it cannot profit regularly.
#
# Code taken from: https://flare9xblog.wordpress.com/2017/09/27/half-life-of-mean-reversion-ornstein-uhlenbeck-formula-for-mean-reverting-process/
# Edited functionality to work for my series
#
# Acceptable values:
# Short Half-life: 1-10 days
# Medium hald life: 10-30 days
# Long hald life: 30-60 days

calculateHalfLife <- function(series) {
  # Calculate yt-1 and (yt-1-yt)
  y.lag <- lag(series, k =1) # Set vector to lag -1 day
  y.lag <- na.omit(y.lag)
  series <- series[2:length(series)] # Make vector same length as vector y.lag
  y.diff <- series - y.lag # Subtract today's close from yesterday's close
  y.diff <- y.diff[1:length(y.diff)-1] # Make vector same length as vector y.lag
  prev.y.mean <- y.lag - mean(y.lag) # Subtract yesterday's close from the mean of lagged differences
  prev.y.mean <- prev.y.mean[1:length(prev.y.mean)-1] # Make vector same length as vector y.lag
  final.df <- as.data.frame(series) # Create final data frame
  
  # Linear Regression With Intercept
  result <- lm(y.diff ~ prev.y.mean, data = final.df)
  half_life <- -log(2) / coef(result)[2]
  
  # Linear Regression With No Intercept
  result1 <- lm(y.diff ~ prev.y.mean + 0, data = final.df)
  half_life1 <- -log(2) / coef(result1)[1]
  
  return(list(HalfLife_WithIntercept = half_life, HalfLife_NoIntercept = half_life1))
}




#-------------------------------------------------------------------------------
