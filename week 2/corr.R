corr <- function(directory, threshold = 0) {
  # Function that takes a directory of data files and a threshold for complete cases and calculates the 
  # correlation between sulfate and nitrate for monitor locations where the number of completely 
  # observed cases (on all variables) is greater than the threshold. 
  #
  # Args:
  #   directory: a character vector of length 1 indicating the location of the CSV files.
  #   threshold: is a numeric vector of length 1 indicating the number of completely observed 
  #   observations required to compute the correlation between nitrate and sulfate; default is 0
  #
  # Returns:
  #   A vector of correlations for the monitors that meet the threshold requirement. 
  #   If no monitors meet the threshold requirement, then the function return a numeric vector of length 0

  # A character vector of the full file paths in the directory  
  fileList <- list.files(directory, full.names = TRUE)
  
  # Use lapply to read the data from all of the CSVs into a list
  pollutionDataList <- lapply(fileList, read.csv, header = TRUE)
  
  # Use lapply to run complete.cases and store the result in validPollutionDataList
  validPollutionDataList <- lapply(pollutionDataList, function(x) x[complete.cases(x),])

  if (is.null(validPollutionDataList) == TRUE){
    # Nothing left after complete.cases so return an empty numeric vector
    numeric(0)
  }
  else {
    # Initialise an empty numeric vector
    correlations <- numeric(0)

    # Step through the validPollutionDataList   
    for (i in seq_along(validPollutionDataList))
      if (nrow(validPollutionDataList[[i]]) >= threshold){
        # Apply correlations to the data frames which pass the threshold test
        correlations <- c(correlations, cor(validPollutionDataList[[i]]$sulfate, validPollutionDataList[[i]]$nitrate))
      }
      
    }
    return(correlations)
  
}