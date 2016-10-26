
url < -"https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
directory <- "John Hopkins Data science/R Programming/Week 4"
path <- paste(getwd(),directory,sep="/")
print(path)
filename <- "rprog_data_ProgAssignment3-data.zip"
fullname <- paste(path,filename,sep="/")
download.file(url,destfile = fullname,method="curl") #curl is to be used only for mac OS
dateDownloaded <- date()

## Unzip teh files
unzip(fullname,exdir = path)

## View the files which have been unzipped
list.files(path)

## Read the outcome data into R via the read.csv function and look at the 1st few rows
outcome <- read.csv(paste(path,"outcome-of-care-measures.csv",sep="/"), colClasses = "character")
head(outcome)

## Find no. of columns in the dataset
ncol(outcome)

## plot histogram of the 30-day death rates from heart attack(11th column)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

## Find the best hospital in the state based on lowest 30 day mortality rate.If there is a tie-sort alphabetically

best <- function(state, outcome) {
  ## Read outcome data
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
  
  #get rid of the NA's in the desired outcome column
  required_columns <- as.numeric(new_data[,outcome_column])
  bad <- is.na(required_columns)
  desired_data <- new_data[!bad, ]
  
  #find the hospitals in the rows with the minimum outcome value
  columns_considered <- as.numeric(desired_data[, outcome_column])
  desired_rows <- which(columns_considered == min(columns_considered))
  desired_hospitals <- desired_data[desired_rows, 2]
  
  #if there are multiple hospitals with the minimum outcome value, then
  #return the first hospital name from the alphabetically sorted hospital
  #names list
  if (length(desired_hospitals) > 1) {
    hospitals_sorted <- sort(desired_hospitals)
    hospitals_sorted[1]
  }
  else {
    desired_hospitals
  }
}

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
