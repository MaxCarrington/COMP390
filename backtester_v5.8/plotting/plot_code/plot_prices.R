library(quantmod)
# Function to create a candlestick chart using base R plotting
createCandlestickChart <- function(price_series) {
  if (!inherits(price_series, "xts")) {
    stop("price_series must be an xts object.")
  }
  
  # Convert xts object to a data frame
  df <- data.frame(Date = index(price_series), coredata(price_series))
  
  # Create a plot
  plot(x = df$Date, y = df$Close, type = "n", xlab = "Date", ylab = "Price", main = "Candlestick Chart")
  # Adding candlesticks
  candlesticks <- function(i) {
    if (df$Close[i] >= df$Open[i]) {
      rect(df$Date[i] - 0.3, df$Open[i], df$Date[i] + 0.3, df$Close[i], col = "green", border = "black")
    } else {
      rect(df$Date[i] - 0.3, df$Close[i], df$Date[i] + 0.3, df$Open[i], col = "red", border = "black")
    }
  }
  sapply(1:nrow(df), candlesticks)
}

# Function to create a base R line plot for ATR or VIX
createLinePlot <- function(series, title, color) {
  if (!inherits(series, "xts") && !inherits(series, "zoo")) {
    stop("series must be an xts or zoo object.")
  }
  plot(index(series), coredata(series), type = "l", col = color, xlab = "Date", ylab = title, main = title)
}

# Function to plot all three together
plotAll <- function(price_series, atr_series, vix_series) {
  # Open a new plot window
  par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
  
  # Plot the candlestick chart
  createCandlestickChart(price_series)
  
  # Plot ATR
  createLinePlot(atr_series, "ATR", "red")
  
  # Plot VIX
  createLinePlot(vix_series, "VIX", "blue")
}

# Usage example
#vix <- weeklyVIXs[[1]]
#atr <- weeklyATRs[[1]]
#print(weeklyATRs[[1]])
#plotAll(series, atr, vix)

