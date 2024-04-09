#-------------------------------------------------------------------------------
#  Calculates the Kelly Formula for previous trades, giving a new position size
#-------------------------------------------------------------------------------

# This environment stores shared variables
positionSizingEnv <- new.env()
positionSizingEnv$winProb <- 0
positionSizingEnv$winLossRatio <- 0

# The position size is calculated using the kelly formula. Giving a percentage
# of the portfolio to invest into the a trade (position size)
calculatePositionSize <- function() {
  w <- positionSizingEnv$winProb
  r <- positionSizingEnv$winLossRatio
  kellyFraction <- w - ((1 - w) / r)
  #Need to ensure that the position size is not greater than 20% of the portfolio
  return(min(kellyFraction, 0.2))
}

# Updates the winning probability
updateProbability <- function(newProbability) {
  positionSizingEnv$winProb <- newProbability
}

# Updates the win/loss ratio
updateWinLossRatio <- function(newRatio) {
  positionSizingEnv$winLossRatio <- newRatio
}

#Analyses previous trades, updating the W and R in the kelly formula 
analysePreviousTrades <- function(previousTrades){
  
  if(length(previousTrades$wins) == 0) {
    winProb <- 0  # No wins means 0% win probability
    winLossRatio <- 100  # Arbitrarily high win/loss ratio when no wins
  } else if(length(previousTrades$losses) == 0) {
    winProb <- 1  # 100 percent win prob as 0 losses
    winLossRatio <- 100  # Very high ratio
  } else {
    winProb <- length(previousTrades$wins) / (length(previousTrades$wins) + length(previousTrades$losses))
    winLossRatio <- mean(previousTrades$wins) / -mean(previousTrades$losses)
  }
  updateProbability(winProb)
  updateWinLossRatio(winLossRatio)
}




#-------------------------------------------------------------------------------