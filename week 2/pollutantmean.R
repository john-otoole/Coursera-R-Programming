pollutantmean <- function(directory, pollutant, id = 1:332) {
  # Computes the mean of a specified pollutant amongst a specified list of csv files
  #
  # Args:
  #   directory: a character vector of length 1 indicating the location of the CSV files.
  #   pollutant: a character vector of length 1 indicating the name of 
  #               the pollutant for which to calculate the mean; either "sulfate" or "nitrate".
  #   id: an integer vector indicating the monitor ID numbers to be used.
  #
  # Returns:
  #   The mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
  
  pollutantData <- NULL
  
  # Loop through the ids in the input integer vector
  for (i in id) {
    # Left pad the id with zeros and read the data from the csv file
    thisPollutantData <- read.csv(file.path(directory, paste(sprintf("%03d", i), ".csv", sep="")))
    # row bind this thisPollutantData to the full set of pollutantData
    pollutantData <- rbind(pollutantData, thisPollutantData)
  }
  
  # Return the mean value after NAs have been removed.
  return(mean(pollutantData[, pollutant], na.rm=TRUE))

}