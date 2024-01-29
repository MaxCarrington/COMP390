#-------------------------------------------------------------------------------
# Performs an Augmented Dickey fuller test
# Determines if a time series is stationary by testing for a unit root (a null
# hypothesis). If there is a unit root, then the time series may be 
# non-stationary. Alternatively if there is no unit root, this is indicative of
# stationarity.
#-------------------------------------------------------------------------------
if (!require(tseries)) 
  install.packages("tseries")
  library(vrtest)
# Performs an ADF test on a given time series
performADFTest <- function(series) {
  testResult <- adf.test(series, alternative = "stationary")
  return(testResult)
}

#Call with:
#adf_test_open <- lapply(inSampleDataList, function(series){
  #result <- performADFTest(series$Open)
#})
#adf_test_close <- lapply(inSampleDataList, function(series){
  #result <- performADFTest(series$Close)
#})
# Assuming adf_test_open and adf_test_close are lists of ADF test results
#for(i in 1:length(adf_test_open)){
  #adf_open_result <- adf_test_open[[i]]
  #adf_close_result <- adf_test_close[[i]]
  
  #if(!is.na(adf_open_result$p.value) && adf_open_result$p.value < 0.05) {
    #print(paste("Open Time series", i, "is stationary"))
  #} else {
    #print(paste("Open Time series", i, "is not stationary"))
  #}
  
  #if(!is.na(adf_close_result$p.value) && adf_close_result$p.value < 0.05) {
    #print(paste("Close Time series", i, "is stationary"))
  #} else {
    #print(paste("Close Time series", i, "is not stationary"))
  #}
#}
#-------------------------------------------------------------------------------