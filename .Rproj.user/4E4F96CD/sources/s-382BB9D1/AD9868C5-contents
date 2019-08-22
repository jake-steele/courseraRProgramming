pollutantmean <- function(directory, pollutant, id = 1:332) {
  threeDigNumVec <- sprintf("%03d", id)
  filesToBeReadVec <- character()
  for (i in seq_along(threeDigNumVec)) {
    filesToBeReadVec[i] <- paste(directory, "/", threeDigNumVec[i], ".csv", sep="")
  }
  
  pollutantVector <- numeric()
  meanOfPollutant <- numeric()
  
  for (j in seq_along(filesToBeReadVec)) {
    currCSV <- read.csv(file=filesToBeReadVec[j], header=TRUE, sep=",")
    pollutantVector <- c(pollutantVector, currCSV[[pollutant]])
  }
  
  meanOfPollutant <- mean(pollutantVector, na.rm=TRUE)
  
  meanOfPollutant
}

complete <- function(directory, id = 1:332) {
  threeDigNumVec <- sprintf("%03d", id)
  filesToBeReadVec <- character()
  for (i in seq_along(threeDigNumVec)) {
    filesToBeReadVec[i] <- paste(directory, "/", threeDigNumVec[i], ".csv", sep="")
  }
  
  nobsVec <- numeric()
  for (j in seq_along(filesToBeReadVec)) {
    currCSV <- read.csv(file=filesToBeReadVec[j], header=TRUE, sep=",")
    currNobsSum <- nrow(currCSV[complete.cases(currCSV), ])
    nobsVec <- c(nobsVec, currNobsSum)
  }
  
  completeDataFrame <- data.frame(id = id, nobs = nobsVec)
  completeDataFrame
  
}