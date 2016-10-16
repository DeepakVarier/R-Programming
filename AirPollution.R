# Air Pollution data


setwd("C:/Users/Deepak/Documents/John Hopkins Data science/R Programming/Week 2")
#getwd()
## Print the contents of the path(default is getwd())
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#Create function to read multiple csv files from a directory
pollutantmean <- function(directory,pollutant,id =1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  print("I am in the function")
  filename <- vector(mode="character", length=length(id))
  for(i in seq_along(id)) {
    x <- id[i]
  
    id_string <- toString(x)
    if (x >= 1 && x <= 9) {
      ## If its a single digit, append 2 0s
      monitor <- paste("00", id_string, ".csv", sep="")
      #cat("1st loop ", monitor)
    }
    else if (x >= 10 && x <= 99) {
      ## If 2  digits are present, append 1 0
      id_string <- toString(x)
      monitor <- paste("0", id_string, ".csv", sep="")
      #cat("2nd loop ", monitor)
    }
    else {
      id_string <- toString(x)
      monitor <- paste(id_string, ".csv", sep="") 
      #cat("3rd loop", monitor)
    }
    filename[i] <- monitor      
    if (i==32) {
      cat(filename[i],"\n")
    }
  }

  accumulator <- 0
    total <- 0  
    for(i in filename) {
      
      i <- paste(getwd(),directory,i,sep="/")
      airquality <- read.csv(i)
      good <- complete.cases(airquality[pollutant])
      #good <- complete.cases(airquality[[pollutant]])
      airquality <- airquality[good, ]
      total <- total + nrow(airquality)
      #accumulator <- accumulator + sum(airquality[pollutant], na.rm = TRUE)
      # We need [[]] around pollutant instead of [] since airquality[pollutant]
      # is a data.frame but we need a vector here. Please note that using either
      #[[]] or [] gives the same results as the test cases but only [[]] successfully
      #passes the submit() script
      accumulator <- accumulator + sum(airquality[[pollutant]], na.rm = TRUE)
    }
    accumulator/total
}


pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
