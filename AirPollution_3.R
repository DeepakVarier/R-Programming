# Air Pollution data


setwd("C:/Users/Deepak/Documents/John Hopkins Data science/R Programming/Week 2")
#getwd()
## Print the contents of the path(default is getwd())
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

corr <- function(directory,threshold = 0){
  ## 'directory' is a character of length 1 
  ## indicating the location of the CSV file
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations
  
  cat("I am inside ",directory,"\n")
  
  print(directory)
  complete_table <- complete("specdata", 1:332)
  nobs <- complete_table$nobs
  # find the valid ids
  ids <- complete_table$id[nobs > threshold]
  # get the length of ids vector
  id_len <- length(ids)
  directory = paste(getwd(),directory,sep = "/")
  corr_vector <- rep(0, id_len)
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="/")
  j <- 1
  for(i in ids) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
    j <- j + 1
  }
  result <- corr_vector
  return(result)
}

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))