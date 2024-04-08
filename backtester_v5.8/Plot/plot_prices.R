library(quantmod)

createCandlestickChart <- function(price_series) {
  if (!inherits(price_series, "xts")) {
    stop("price_series must be an xts object.")
  }
  
  df <- data.frame(Date = index(price_series), coredata(price_series))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close") # assuming the price series has these columns
  
  plot(x = df$Date, y = df$Close, type = "n", xlab = "Date", ylab = "Price", main = "Candlestick Chart")
  
  for (i in 1:nrow(df)) {
    if (df$Close[i] >= df$Open[i]) {
      rect(df$Date[i] - 0.3, df$Open[i], df$Date[i] + 0.3, df$Close[i], col = "green", border = "black")
    } else {
      rect(df$Date[i] - 0.3, df$Close[i], df$Date[i] + 0.3, df$Open[i], col = "red", border = "black")
    }
  }
  
  # Now add the x-axis with monthly intervals
  axis(1, at = seq(from=min(df$Date), to=max(df$Date), by="months"), labels = FALSE)
}

createLinePlot <- function(series, title, color) {
  if (!inherits(series, "xts") && !inherits(series, "zoo")) {
    stop("series must be an xts or zoo object.")
  }
  
  plot(index(series), coredata(series), type = "l", col = color, xlab = "Date", ylab = title, main = title)
  
  # Add x-axis with monthly intervals
  axis(1, at = seq(from=min(index(series)), to=max(index(series)), by="months"), labels = FALSE)
}

plotAll <- function(price_series, atr_series, vix_series, combined_ATRVIX) {
  # Open a new plot window
  par(mfrow = c(4, 1), mar = c(4, 4, 2, 1))
  
  # Plot the candlestick chart
  createCandlestickChart(price_series)
  
  # Plot ATR
  createLinePlot(atr_series, "ATR", "red")
  
  # Plot VIX
  createLinePlot(vix_series, "VIX", "blue")
  
  # Plot Combined ATR-VIX
  createLinePlot(combined_ATRVIX, "Combined ATR-VIX", "purple")
}
