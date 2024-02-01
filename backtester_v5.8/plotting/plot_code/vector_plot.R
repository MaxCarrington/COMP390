# Function to plot a list of correlations
plotCorrelations <- function(correlations, title = "Rolling Correlations") {
  pdf("/Users/maxcarrington/Documents/COMP390/Code/backtester_v5.8/plotting/plots/futureReturnCorr.pdf", width = 8.3, height = 11.7)
  n <- length(correlations)
  par(mfrow=c(n, 1))  # Set up a multi-paneled plot with 'n' rows
  
  for (i in 1:n) {
    par(mar = c(3, 2, 2, 3))  # Set equal margins
    plot(correlations[[i]], type = "l",
         xlab = "Time Period", ylab = "Correlation",
         main = paste(title, " (Plot ", i, ")", sep = ""))
  }
  
  par(mfrow=c(1, 1))  # Reset the plot layout to a single pane
  dev.off()
}
plotVolumeTrendAnalysis <- function(rollingAverageVol, rollAvgPriceInc, correlation){
  par(mfrow=c(3, 1), mar=c(4, 4, 2, 1))  # 3 rows, 1 column, adjust margins
  
  # Plot 1: Rolling Average of Trading Volume (Barplot)
  barplot(rollingAverageVol, main="Rolling Average of Trading Volume", xlab="", ylab="Volume")
  
  # Plot 2: Rolling Mean of Price Increases (Line plot)
  plot(rollAvgPriceInc, type="l", main="Rolling Mean of Price Increases", xlab="", ylab="Mean Increase")
  
  # Plot 3: Correlation Scatterplot
  plot(rollingAverageVol, rollAvgPriceInc, main="Correlation between Volume and Price Increases",
       xlab="Volume", ylab="Mean Increase")
  # Round the correlation values to 3 decimal places
  roundedCorrelation <- round(correlation, 3)
  
  # Label points with rounded correlation values
  text(rollingAverageVol, rollAvgPriceInc, labels=roundedCorrelation, pos=3)
  
  # Reset the plot layout to a single pane
  par(mfrow=c(1, 1))
}
