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

corr <- function(directory, threshold = 0) {
  corrVec <- numeric()
  completeDataFrame <- complete(directory)
  
  fileNames <- Sys.glob(paste(directory, "/", "*.csv", sep=""))
  for (i in 1:nrow(completeDataFrame)) {
    if (completeDataFrame[i, 2] >= threshold) {
      currFile <- read.csv(fileNames[i], header=TRUE, sep=",")
      currFile <- currFile[complete.cases(currFile), ]
      corrVec <- c(corrVec, cor(currFile$sulfate, currFile$nitrate))
    }
  }
  
  corrVec
  
}