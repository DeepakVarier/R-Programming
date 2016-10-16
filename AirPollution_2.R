# Air Pollution data


setwd("C:/Users/Deepak/Documents/John Hopkins Data science/R Programming/Week 2")
#getwd()
## Print the contents of the path(default is getwd())
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character of length 1 
  ## indicating the location of the CSV file
  
  ## 'id' is an integer vector 
  ## indicating the monitor ID numbers to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1 117
  ## 2 1041
  ## ...
  ## where 'id' is the monitor ID number and 
  ## 'nobs' is the number of complete cases

  cat("I am inside ",directory,"\n")
  directory = paste(getwd(),directory,sep = "/")
  print(directory)
  allfiles <- list.files(path=directory,full.names = TRUE)
  nobs = rep(0, length(id))
  k <- 1
  for(i in id){
    print(allfiles[i])
    airpol <- read.csv(allfiles[i])
    nobs[k] <- sum(complete.cases(airpol))
    k <- k + 1
  }
  returnVal <- data.frame(id, nobs)

}
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
