for (i in 1:length(results$pnlList)) {
  cat("Time Series:", i, "\n")
  cat("Final Cumulative PnL:", tail(results$pnlList[[i]]$CumPnL, 1), "\n")
}

#Print the final account balance
final_balance <- pfolioPnL$pfoliosPnL$CumPnL[nrow(pfolioPnL$pfoliosPnL)]
cat("Portfolio Profit and Loss: ", round(final_balance, digits=2), "\n")