plotCandlestickWithIndex <- function(series, timeFrame, vix = NULL, atr = NULL) {
  # Load the quantmod library if not already loaded
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    install.packages("quantmod")
  }
  library(quantmod)
  
  # Assuming 'series' is your xts object with columns Open, High, Low, Close
  series_xts <- series
  
  # Create a candlestick chart based on the time frame using 'dates' as the order.by argument
  chartData <- to.period(series_xts, period = timeFrame, indexAt = "endof")
  
  # Create a layout for multi-panel plots
  layout(matrix(c(1, 1, 2, 2), nrow = 2, byrow = TRUE), heights = c(3, 1))
  
  # Plot the candlestick chart in the first panel
  chartSeries(chartData, theme = chartTheme("white"), multi.panel = TRUE, name = "Candlesticks")
  
  # Plot the ATR in the second panel (if provided)
  if (!is.null(atr)) {
    plot(atr, type = "l", col = "blue", ylab = "ATR", xaxt = "n")
  }
  
  # Plot the VIX in the third panel (if provided)
  if (!is.null(vix)) {
    par(new = TRUE)
    plot(vix, type = "l", col = "red", ylab = "VIX", xaxt = "n")
  }
  
  # Add x-axis labels
  axis(1, at = index(chartData), labels = TRUE)
}
