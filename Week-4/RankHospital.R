## Ranking Hospital based on outcome
rankhospital <- function(state,outcome,num){
  
  ## Read outcome data
  path <- paste(getwd(),directory,sep="/")
  data <- read.csv(paste(path,"outcome-of-care-measures.csv",sep="/"), colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- data[ , 7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if ((state %in% states) == FALSE) {
    stop(print("invalid state"))
  }
  else if ((outcome %in% outcomes) == FALSE) {
    stop(print("invalid outcome"))
  }
  ## Get the outcome data for the specific state
  new_data <- subset(data, State == state)
  if (outcome == "heart attack") {
    outcome_column <- 11
  }
  else if (outcome == "heart failure") {
    outcome_column <- 17
  }
  else {
    outcome_column <- 23
  }
  
  #if num is greater that the number of hospitals in the desired state,
  # return NA
  if (is.numeric(num) == TRUE) {
    if (length(new_data[,2]) < num) {
      return(NA)
    }
  }
  
  #get rid of the NA's in the desired outcome column
  #required_columns <- as.numeric(new_data[,outcome_column])
  #bad <- is.na(required_columns)
  #desired_data <- new_data[!bad, ]
  desired_data <- na.omit(new_data)
  
  #find the hospitals in the rows with the minimum outcome value
  columns_considered <- as.numeric(desired_data[, outcome_column])
  desired_rows <- which(columns_considered == min(columns_considered))
  desired_hospitals <- desired_data[desired_rows, 2]
  
  #arrange the modified dataframe in ascending order of the outcome values
  outcome_column_name <- names(desired_data)[outcome_column]
  hospital_column_name <- names(desired_data)[2]
  index <- with(desired_data, order(desired_data[outcome_column_name], desired_data[hospital_column_name]))
  ordered_desired_data <- desired_data[index, ]
  
  #if nume is either "best" or "worst", then interpret it to the
  #corresponding numerical value
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(ordered_desired_data[, outcome_column])
    }
  }
  #return the hospital name with the outcome ranking of num
  ordered_desired_data[num, 2]
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)