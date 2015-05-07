pollutant_correlation <- function(filename) {
  # Given a file, compute the correlation between pollutants
  
  df = read.csv(filename)
  return(cor(df["sulfate"], df["nitrate"], use = "complete.obs"))
  
}
corr <- function(directory, threshold = 0) {
  # Get number of observations per station
  allStations = complete(directory, 1:332)
  # Create DF for only those stations above the threshold
  okStations = allStations[allStations["nobs"] > threshold,]
  
  # If empty, then no stations pass the threshold.
  # Return empty numeric vector
  if (nrow(okStations) == 0) {
    return(numeric())
  }
  # By default, take all files
  c <- sprintf("%03d", unlist(okStations["id"]))
  csv_files = paste(directory, "/", c,".csv", sep = "")
  
  # Take correlations for files that meet the threshold
  result = lapply(csv_files, pollutant_correlation)
  return(unlist(result))
}