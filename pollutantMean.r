compute <- function(filename, pollutant) {
  ### given a csv filename, return the average pollutant
  # columns are 'sulfate', 'nitrate', 'ID' ###
  
  df = read.csv(filename)
  
  # New DataFrame where only valid rows are present
  valid_df = df[!is.na(df[pollutant]),]
  
  # If the resutant is not a null dataframe, there is at least one valid row
  if (nrow(valid_df) > 0) {
    # Mean of the selected column
    total = sum(valid_df[pollutant])
    len = nrow(valid_df)
    return(c(total,len))
  }
  # If empty dataframe, then return 0
  else {
    return(c(0,0))
  }
}

## AVERAGE POLLUTANTS
pollutantmean <- function(directory, pollutant, id=1:332) {
  ### Convert the integer ID to three digit filename: 001.csv ###
  
  # By default, take all files
  c <- sprintf("%03d", id)
  csv_files = paste(directory, "/", c,".csv", sep = "")
  
  # Take average across all files specified
  total_pollutant = 0
  total_count = 0
  for (file in csv_files) {
    print(file)
     site_values = compute(file, pollutant)
     total_pollutant = total_pollutant + site_values[1]
     total_count = total_count + site_values[2]
  }
  avg_pollutant = total_pollutant / total_count
  return(avg_pollutant)
}

