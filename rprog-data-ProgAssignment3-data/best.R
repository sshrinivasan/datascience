best <- function(state, outcome) {

  # Column containing hospital
  hospitalColumn <- 2
  # Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  # Get all valid states
  all_states <- unique(df[, "State"])

  # All valid outcomes
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

  # Error check the inputs
  if(!(state %in% all_states)) {
    stop("invalid state")
  }

  if(!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }


  # Select the column to loop for data
  if(outcome == "heart attack") col <- 11
  if(outcome == "heart failure") col <- 17
  if(outcome == "pneumonia") col <- 23

  suppressWarnings(df[, col] <- as.numeric(df[, col]))

  # Get only hospitals in that state
  state_df <- subset(df, df["State"] == state)

  # Get the lowest mortality value
  min_mortality <- min(state_df[,col], na.rm = TRUE)

  # All hospitals with this lowest mortality
  low_df <- subset(state_df, state_df[col] == min_mortality)

  # If multiple results match the lowest mortality, sort the dataframe by Hospital.Name
  if(nrow(low_df) > 1) {
    print("Multiple results found")
    print(low_df[, c(2, col)])
    low_df <- low_df[order(low_df[hospitalColumn]),]
  }
  # Return the first row element
  print(low_df[1, hospitalColumn])
}