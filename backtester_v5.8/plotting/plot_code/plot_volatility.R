plotATRandVIXComparison <- function(atrList, vixList) {
  # Create a list of data frames for each ATR and VIX pair
  data_frames <- lapply(1:10, function(i) {
    x <- seq(1, length(atrList[[i]]))
    df <- data.frame(x = x, ATR = atrList[[i]], VIX = vixList[[i]])
    return(df)
  })
  
  # Create a list of plots
  plots <- lapply(data_frames, function(df) {
    ggplot(df, aes(x = x)) +
      geom_line(aes(y = ATR, color = "ATR")) +
      scale_color_manual(values = c("ATR" = "blue")) +
      geom_line(aes(y = VIX * 10, color = "VIX")) +
      scale_color_manual(values = c("VIX" = "red")) +
      labs(x = "Time", y = "ATR", color = "Legend") +
      scale_y_continuous(
        sec.axis = sec_axis(~./10, name = "VIX"),
        breaks = seq(0, max(df$VIX * 10), by = 10)
      ) +
      theme_minimal()
  })
  grid.arrange(grobs = plots, ncol = 2)  # Change ncol to the desired number of columns
}

plotATRData <- function(weeklyFile, monthlyFile, fortnightlyFile, titleString = NULL) {
  # Load data from RDS files
  weeklyATRs <- readRDS(weeklyFile)
  monthlyATRs <- readRDS(monthlyFile)
  fortnightlyATRs <- readRDS(fortnightlyFile)
  
  nseries <- length(weeklyATRs)
  plots <- list()
  
  for(i in seq_len(nseries)) {
    # Total number of weeks in the weekly ATRs for this series
    totalWeeks <- length(weeklyATRs[[i]])
    
    # Creating individual data frames for each period
    dataWeekly <- data.frame(Time = seq_len(totalWeeks), 
                             ATR = weeklyATRs[[i]], 
                             Period = "Weekly")
    
    # Adjust monthly and fortnightly data to match the same timeframe as weekly data
    dataMonthly <- data.frame(Time = seq(from = 4, length.out = length(monthlyATRs[[i]]), by = 4),
                              ATR = monthlyATRs[[i]], 
                              Period = "Monthly")
    dataFortnightly <- data.frame(Time = seq(from = 2, length.out = length(fortnightlyATRs[[i]]), by = 2),
                                  ATR = fortnightlyATRs[[i]], 
                                  Period = "Fortnightly")
    
    # Combining data frames
    combinedData <- rbind(dataWeekly, dataMonthly, dataFortnightly)
    
    # Create plot for each series
    plots[[i]] <- ggplot(combinedData, aes(x = Time, y = ATR, color = Period)) +
      geom_line() +
      labs(title = paste("Series", i, "ATR"), x = "Time (Weeks)", y = "ATR") +
      theme_minimal() +
      scale_color_manual(values = c("Weekly" = "red", "Monthly" = "blue", "Fortnightly" = "green")) +
      ggtitle(titleString) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # Arrange all plots in a grid
  grid.arrange(grobs = plots, ncol = 2, top = textGrob(titleString, gp = gpar(fontsize = 20, font = 1)))
}
plotVIXData <- function(weeklyFile, monthlyFile, fortnightlyFile, titleString = NULL) {
  # Load data from RDS files
  weeklyVIXs <- readRDS(weeklyFile)
  monthlyVIXs <- readRDS(monthlyFile)
  fortnightlyVIXs <- readRDS(fortnightlyFile)
  
  nseries <- length(weeklyVIXs)
  plots <- list()
  
  for(i in seq_len(nseries)) {
    # Total number of weeks in the weekly VIXs for this series
    totalWeeks <- length(weeklyVIXs[[i]])
    
    # Creating individual data frames for each period
    dataWeekly <- data.frame(Time = seq_len(totalWeeks), 
                             VIX = weeklyVIXs[[i]], 
                             Period = "Weekly")
    dataMonthly <- data.frame(Time = seq(from = 4, length.out = length(monthlyVIXs[[i]]), by = 4),
                              VIX = monthlyVIXs[[i]], 
                              Period = "Monthly")
    dataFortnightly <- data.frame(Time = seq(from = 2, length.out = length(fortnightlyVIXs[[i]]), by = 2),
                                  VIX = fortnightlyVIXs[[i]], 
                                  Period = "Fortnightly")
    
    # Combining data frames
    combinedData <- rbind(dataWeekly, dataMonthly, dataFortnightly)
    
    # Create plot for each series
    plots[[i]] <- ggplot(combinedData, aes(x = Time, y = VIX, color = Period)) +
      geom_line() +
      labs(title = paste("Series", i, "VIX"), x = "Time (Weeks)", y = "VIX") +
      theme_minimal() +
      scale_color_manual(values = c("Weekly" = "red", "Monthly" = "blue", "Fortnightly" = "green")) +
      ggtitle(titleString) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # Arrange all plots in a grid
  grid.arrange(grobs = plots, ncol = 2, top = textGrob(titleString, gp = gpar(fontsize = 20, font = 1)))
}





