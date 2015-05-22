rankhospital <- function(state, outcome, num = "best") {
  # Column containing hospital
  hospitalColumn <- 2
  # Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  # Choose the ranking
  if (num == "best") ranking <- 1
  else if (num == "worst") ranking <-  -1
  else ranking <- num

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

  # Remove rows with NA for that particular outcome (not complete.cases across all columns)
  state_df <- state_df[complete.cases(state_df[, col]),]

  # Order them by mortality, then by hospital name
  state_df <- state_df[order(state_df[col], state_df[hospitalColumn]),]

  if (num == "best") {
      print(state_df[1, hospitalColumn])
  }
  else if (num == "worst") {
      print(tail(state_df[, hospitalColumn], 1))
  }
  else if (is.numeric(num)){
      print(state_df[num, hospitalColumn])
  }
  else {
    stop("Invalid ranking specified")
  }
}
