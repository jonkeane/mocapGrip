process <- function(file, conditionCodesFile, verbose=FALSE){
  # read in csvs as strings including NA (which indicates occlusion) as a string (and empty as a true NA)
  # readr::read_csv doesn't work as a simple replacement.
  data <- utils::read.csv(file=file, na.strings = "", stringsAsFactors=FALSE)
  data$times <- seq(from = 0, to = (1/120)*(nrow(data)-1), length.out= nrow(data))
  # melt ignoring true NAs
  meltedData <- reshape2::melt(data, id=c("times"), na.rm = TRUE, value.name = "grip")

  #Turn string NAs (occlusions) into real NAs, and then numericize
  meltedData$grip <- ifelse(meltedData$grip=="NA",NA,meltedData$grip)
  meltedData$grip <- as.numeric(meltedData$grip)

  meltedData$variable <- as.character(meltedData$variable)
  meltedData$variable <- as.factor(ifelse(grepl(".*\\..$",meltedData$variable),meltedData$variable,paste(meltedData$variable,"0",sep=".")))

  meltedData$variable <- gsub("X","",meltedData$variable)
  meltedData$variable <- gsub("(\\.)+",".",meltedData$variable)

  if(verbose){print(file)}
  if(verbose){print(unique(meltedData$variable))}
  #   meltedData <- meltedData %>% separate(variable, into=c("condition","type","side","period","num"), sep="\\.", extra = "drop")

  meltedData <- meltedData %>% tidyr::extract_("variable", into=c("condition","type","period","gripType","num"), regex="([[:digit:]][[:digit:]]?)\\.(ACTION|GESTURE|ESTIMATION)\\.(EYESCLOSED|OBSERVE|GRIP|MOVEMENT|RELEASE|PLANNING|PREPARE|STEADY|TRANSITION|UNCODABLE|NO.GESTURE|TRANSITION.GRIP)\\.?(CLOSED|OPEN|OPEN.CLOSED|CLOSED.OPEN)?\\.([[:digit:]])", convert=TRUE)

  meltedData$condition <- as.factor(meltedData$condition)
  meltedData$type <- as.factor(meltedData$type)
  meltedData$period <- as.factor(meltedData$period)

  meltedData$file <- file

  meltedData <- meltedData %>% tidyr::separate_("file", into=c("obsisSubj","obsisSession","obsisTrial"), sep="-", remove=FALSE)

  meltedData$obsisSubj <- sub(".*GRI_([[:digit:]][[:digit:]][[:digit:]])", "\\1",  meltedData$obsisSubj)

  meltedData$obsisSession <- gsub("SESSION_", "", meltedData$obsisSession)
  meltedData$obsisTrial <- gsub("TRIAL_", "", meltedData$obsisTrial)
  meltedData$obsisTrial <- gsub(".csv", "", meltedData$obsisTrial)
  meltedData$obsisTrial <- gsub("TEST", "", meltedData$obsisTrial)

  meltedData$file <- as.factor(meltedData$file)

  condCodes <- utils::read.csv(conditionCodesFile)

  # change to dplyr inner_join?
  meltedData <- plyr::join(meltedData, condCodes, by="condition")

  meltedData
}

# grab only grip and time data
importCols <- function(data){
  # return data without the times or grip column
  data[ , !(names(data) %in% c("times", "grip"))]
}

#' Processing function for finding maximum grips
#'
#' \code{maxGripFinder()} takes the period and extracts the maximum value of grip for the whole period.
#' This function should only be used in \code{\link{processDataSet}}, because that has the code to group all
#'  of the data into the right groups based on subject, session, trial, condition, period, etc. This should be
#'  included in the processing section of \link{modelMetadata} called \code{processFunction}
#'
#' @param data the data to process.
#' @param percOcclusion the percentage of occlusion that is acceptable (this is the upper bound, in percent.) Default: \code{0.05}, or only trials where there is less than 5\% of occlusion are processed.
#'
#' @return a dataframe with the data that has been processed, one line per observation
#'
maxGripFinder <- function(data, percOcclusion = 0.05) {
  if(nrow(data) == 0 ){
    warning(paste("There was no GRIP period found for obsisSubj:", unique(data$obsisSubj), "obsisTrail:", unique(data$obsisTrial), "condition", unique(data$condition), "trial type:", unique(data$type), "The following periods were found:", paste(unique(data$period), collapse=", "),  sep =" "))
    return(data.frame())
  }
  if(sum(is.na(data$grip))/nrow(data) > percOcclusion ){
    warning(paste("There is ", as.character(round(sum(is.na(data$grip))/nrow(data)*100, digits=4)), "% occlusion for obsisSubj: ", unique(data$obsisSubj), " obsisTrail: ", unique(data$obsisTrial), " condition: ", unique(data$condition), " period: ", unique(data$period),  " trial type: ", unique(data$type),  sep =""))
    return(data.frame())
  }
  maxTime <- max(data$times, na.rm = TRUE)
  # add logic to catch multiple maximums
  maxGripRow <- data[which.max(data$grip),]

  return(cbind(data.frame(duration=maxTime, maxGrip=maxGripRow$grip, maxGripTime=maxGripRow$times, maxGripTimeRel=maxGripRow$times/maxTime), importCols(maxGripRow)))
}


#' Processing function for finding means or medians
#'
#' \code{meanMedianFinder()} takes the dataSet and extracts the mean and median of grip for the whole dataSet.
#' This function should only be used in \code{\link{processDataSet}}, because that has the code to group all
#'  of the data into the right groups based on subject, session, trial, condition, period, etc. This should be
#'  included in the processing section of \link{modelMetadata} called \code{processFunction}
#'
#' @param data the data to process.
#' @param percOcclusion the percentage of occlusion that is acceptable (this is the upper bound, in percent.) Default: \code{0.05}, or only trials where there is less than 5\% of occlusion are processed.
#'
#' @return a dataframe with the data that has been processed, one line per observation
#'
meanMedianFinder <- function(data, percOcclusion = 0.05) {
  if(nrow(data) == 0 ){
    warning(paste("There was no STEADY period found for obsisSubj:", unique(data$obsisSubj), "obsisTrail:", unique(data$obsisTrial), "condition", unique(data$condition), "trial type:", unique(data$type), "The following periods were found:", paste(unique(data$period), collapse=", "),  sep =" "))
    return(data.frame())
  }
  if(sum(is.na(data$grip))/nrow(data) > percOcclusion ){
    warning(paste("There is ", as.character(round(sum(is.na(data$grip))/nrow(data)*100), digits=4), "% occlusion for obsisSubj: ", unique(data$obsisSubj), " obsisTrail: ", unique(data$obsisTrial), " condition: ", unique(data$condition), " period: ", unique(data$period), " trial type: ", unique(data$type),  sep =""))
    return(data.frame())
  }

  maxTime <- max(data$times, na.rm=TRUE)
  meanGrip <- mean(data$grip, na.rm=TRUE)
  medianGrip <- stats::median(data$grip, na.rm=TRUE)

  return(cbind(data.frame(duration=maxTime, meanGrip=meanGrip, medianGrip=medianGrip), importCols(data[1,])))
}


#' takes a dataSet and data, and gives back a list with the extracted data in named lists.
#'
#' @param dataSet character the name of the dataSet
#' @param data a data object for the data to be retrieved from
#' @param modelMd a modelMetadata object, default: the modelMetadata from the package
#'
#' @return the extracted data from the dataSet
processDataSet <- function(dataSet, data, modelMd = modelMetadata){
  # make a list to store dataSet data in
  dataSetData <- list()

  # make a list for warnings to be stored
  warns <- list()

  # grab the dataSet specificationsi from the modelMd object
  filterString <- modelMd$dataSets[[dataSet]]$processing$filterString
  func <- modelMd$dataSets[[dataSet]]$processing$processFunction
  percentOcclusion <- modelMd$dataSets[[dataSet]]$processing$percentOcclusion

  # Try and find a the given processing file
  # the error could be more specific
  # asNamespace might not be needed.
  tryCatch(
    {get(func, envir=asNamespace('mocapGrip'), mode='function')},
    error = function(e) {
      stop("Could not find the function ", func, ", which was specified to process the data for the dataSet ", dataSet, sep="")
    }
  )

  # process the data with the function that was found, add it to dataSetData (along with warnings)
  dataSetData[["data"]] <- withCallingHandlers({
    data %>%
      dplyr::filter_(stats::as.formula(paste0("~", filterString))) %>%
      dplyr::group_by_(.dots=list("obsisSubj","obsisTrial","condition")) %>%
      dplyr::do_(stats::as.formula(paste0("~", func, "(., percOcclusion = ", percentOcclusion, ")")))
    },
    warning = function(w) {
      warns <<- append(warns,w$message)
      invokeRestart("muffleWarning")
    }
  )
  dataSetData[["warnings"]] <- warns

  # grab default analyses
  dataSetData[["analysesToRun"]] <- modelMd$dataSets[[dataSet]]$defaultAnalysis

  return(dataSetData)
}



#' Read extracted motion capture data
#'
#' Reads in extracted motion capture data from a directory.
#'
#' @param path Directory containing motion catpure csv files that were extracted with the \code{\link{extractMocapDataFromAnnotations}} function.
#' @param dataSets A vector of the types of periods (aka: dataSets) to extract for analysis. Default: c("action", "estimation") Possible values are: "action", "estimation", "release", "estMaxGrip", "gestMaxGrip", and "gestMove"
#' @param includeFullData A logical, should the output include the full data? default:\code{FALSE}
#' @return Not sure yet.
#'
#' @export
readExtractedMocapData <- function(path, dataSets = c("action", "estimation"), includeFullData=FALSE){
  # to be added to main function for oparsing data
  files <- list.files(path, recursive = TRUE, pattern = NULL, full.names=TRUE)

  data <- plyr::ldply(files, process, conditionCodesFile=system.file("GRIPMLstimuli.csv", package = "mocapGrip", mustWork=TRUE), verbose=FALSE, .progress = "text" )

  # modifications of the data to be better structure (should these go elsewhere?)
  data$stick <- factor(as.character(data$stick), levels = c("five", "seven", "nine", "eleven"))
  data$stickcmScaled <- data$stickcm - 8

  # add check if there are no known types found.
  dataSetData <- sapply(dataSets, processDataSet, data=data, USE.NAMES = TRUE, simplify = FALSE)

  if(includeFullData==TRUE){
    dataSetData[["fullData"]] <- data
  }

  return(dataSetData)
}
