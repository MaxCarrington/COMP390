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
  positiveTrades <- previousTrades$wins
  negativeTrades <- previousTrades$losses
  #Any number above 0.50 is a good winProb
  winProb <- length(positiveTrades) / (length(positiveTrades) + length(negativeTrades))
  winLossRatio <- mean(positiveTrades)/ -mean(negativeTrades) #maybe remove the negative symbol
  updateProbability(winProb)
  updateWinLossRatio(winLossRatio)
}


#-------------------------------------------------------------------------------