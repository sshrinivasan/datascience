# Given a file, return the number of complete cases
complete_cases <- function(filename) {
  df = read.csv(filename)
  return(nrow(df[complete.cases(df),]))
}

## NUMBER OF COMPLETE CASES
complete <- function(directory, id=1:332) {
  
  # By default, take all files
  c <- sprintf("%03d", id)
  csv_files = paste(directory, "/", c, ".csv", sep = "")
  
  # Create a numeric vector to store the counts
  vals = numeric(0)
  for (file in csv_files) {
    cc = complete_cases(file)
    vals <- c(vals, cc)
  }
  # Apply the compelte_cases function to each file
  result = lapply(csv_files, complete_cases)
  # Convert to vector for dataframe creation
  nobs = unlist(result)
  # Create dataframe with two columns
  newdf <- data.frame(id, nobs)
  return(newdf)
}