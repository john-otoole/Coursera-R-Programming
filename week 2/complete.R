complete <- function(directory, id = 1:332) {
  # Read a directory full of files and reports the number of completely observed cases in each data file
  #
  # Args:
  #   directory: a character vector of length 1 indicating the location of the CSV files.
  #   id: an integer vector indicating the monitor ID numbers to be used.
  #
  # Returns:
  #   Return a data frame where the first column is the name of the file and the 
  #   second column is the number of complete cases
  
  
  # Loop through the ids in the input integer vector
  for (i in id) {
    # Left pad the id with zeros and read the data from the csv file
    pollutantData <- read.csv(file.path(directory, paste(sprintf("%03d", i), ".csv", sep="")))

    if (!exists("resultsDataFrame")) {
      resultsDataFrame <- data.frame('id' = i, 'nobs' = nrow(pollutantData[complete.cases(pollutantData), ]))
      
    } else {
      thisResult <- c(i, nrow(pollutantData[complete.cases(pollutantData), ]))
      resultsDataFrame <- rbind(resultsDataFrame, thisResult)
    }
    
  }
    
  return(resultsDataFrame)
}