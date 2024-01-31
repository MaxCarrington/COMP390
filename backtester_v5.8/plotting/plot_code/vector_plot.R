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
