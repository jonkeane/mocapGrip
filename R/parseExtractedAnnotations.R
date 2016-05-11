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




# processing function for steady period
steadyProc <- function(data) {
  # subData <- subset(data, period == "STEADY")
  subData <- data[data$period == "STEADY",]

  if(nrow(subData) == 0 ){
    warning(paste("There was no STEADY period found for obsisSubj:", unique(data$obsisSubj), "obsisTrail:", unique(data$obsisTrial), "condition", unique(data$condition), "trial type:", unique(data$type), "The following periods were found:", paste(unique(data$period), collapse=", "),  sep =" "))
    return(data.frame())
  }
  if(sum(is.na(subData$grip))/nrow(subData) > 0.05 ){
    warning(paste("There is ", as.character(round(sum(is.na(subData$grip))/nrow(subData)*100), digits=4), "% occlusion for obsisSubj: ", unique(data$obsisSubj), " obsisTrail: ", unique(data$obsisTrial), " condition: ", unique(subData$condition), " period: ", unique(subData$period), " trial type: ", unique(subData$type),  sep =""))
    return(data.frame())
  }

  maxTime <- max(subData$times, na.rm=TRUE)
  meanGrip <- mean(subData$grip, na.rm=TRUE)
  medianGrip <- stats::median(subData$grip, na.rm=TRUE)
  cbind(data.frame(duration=maxTime, meanGrip=meanGrip, medianGrip=medianGrip), importCols(subData[1,]))
}

# processing function for movement period
moveProc <- function(data) {
  # subData <- subset(data, period == "MOVEMENT")
  subData <- data[data$period == "MOVEMENT", ]

  if(nrow(subData) == 0 ){
    warning(paste("There was no MOVEMENT period found for obsisSubj:", unique(data$obsisSubj), "obsisTrail:", unique(data$obsisTrial), "condition", unique(data$condition), "trial type:", unique(data$type), "The following periods were found:", paste(unique(data$period), collapse=", "),  sep =" "))
    return(data.frame())
  }
  if(sum(is.na(subData$grip))/nrow(subData) > 0.05 ){
    warning(paste("There is ", as.character(round(sum(is.na(subData$grip))/nrow(subData)*100), digits=4), "% occlusion for obsisSubj: ", unique(data$obsisSubj), " obsisTrail: ", unique(data$obsisTrial), " condition: ", unique(subData$condition), " period: ", unique(subData$period),  " trial type: ", unique(data$type),  sep =""))
    return(data.frame())
  }

  maxTime <- max(subData$times, na.rm=TRUE)
  meanGrip <- mean(subData$grip, na.rm=TRUE)
  medianGrip <- stats::median(subData$grip, na.rm=TRUE)
  cbind(data.frame(duration=maxTime, meanGrip=meanGrip, medianGrip=medianGrip), importCols(subData[1,]))
}



# processing function for maximum grip period
# the percOcclusion variable sets the maximum allowable occlusion, the default is 0.05
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

actionGripProc <-function(data, ...) {
  # extract the maxmium grip during the grip portion of all action trials
  filter_criteria <- lazyeval::interp(~ type == "ACTION" & period == "GRIP", type = as.name("type"), period = as.name("period"))
  data %>% dplyr::filter_(filter_criteria) %>% dplyr::group_by_(.dots=list("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(~maxGripFinder(., ...))
}

releaseGripProc <-function(data, ...) {
  # extract the maxmium grip during the release portion of all action trials
  filter_criteria <- lazyeval::interp(~ type == "ACTION" & period == "RELEASE", type = as.name("type"), period = as.name("period"))
  data %>% dplyr::filter_(filter_criteria) %>% dplyr::group_by_(.dots=list("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(~maxGripFinder(., ...))
}

estMaxGripGripProc <-function(data, ...) {
  # extract the maxmium grip during the grip portion of all estimate trials
  filter_criteria <- lazyeval::interp(~ type == "ESTIMATION" & period == "GRIP", type = as.name("type"), period = as.name("period"))
  data %>% dplyr::filter_(filter_criteria) %>% dplyr::group_by_(.dots=list("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(~maxGripFinder(., ...))
}

gestMaxGripGripProc <-function(data, ...) {
  # extract the maxmium grip during the grip portion of all gesture trials
  # Side information for experiments involving side choices
  # actionSideDF <- data %>% dplyr::filter_("type"=="ACTION") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::summarise_(actionSide=unique("side"))
  filter_criteria <- lazyeval::interp(~ type == "GESTURE" & period == "GRIP", type = as.name("type"), period = as.name("period"))
  data %>% dplyr::filter_(filter_criteria) %>% dplyr::group_by_(.dots=list("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(~maxGripFinder(., ...))
}



# processing function for finding means or medians
# the percOcclusion variable sets the maximum allowable occlusion, the default is 0.05
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

estimationGripProc <-function(data, ...) {
  # extract the mean and median grip during the steady portion of all estimate trials
  filter_criteria <- lazyeval::interp(~ type == "ESTIMATION" & period == "STEADY", type = as.name("type"), period = as.name("period"))
  data %>% dplyr::filter_(filter_criteria) %>% dplyr::group_by_(.dots=list("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(~meanMedianFinder(., ...))
}

gestMoveGripProc <-function(data, ...) {
  # extract the mean and median grip during the move portion of all gesture trials
  # Side information for experiments involving side choices
  # actionSideDF <- data %>% dplyr::filter_("type"=="ACTION") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::summarise_(actionSide=unique("side"))
  filter_criteria <- lazyeval::interp(~ type == "GESTURE" & period == "MOVEMENT", type = as.name("type"), period = as.name("period"))
  data %>% dplyr::filter_(filter_criteria) %>% dplyr::group_by_(.dots=list("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(~meanMedianFinder(., ...))
}




# takes a (vector of) period(s) and data, and gives back a list with the extracted data in named lists.
# this can't be Vectorized (hangs, instead of errors, unclear why)
processPeriod <- function(period, data){
  # make a list to store period data in
  periodData <- list()

  # make a list for warnings to be stored
  warns <- list()

  # Try and find a processing function with is nameGripProc(). Give an error if none is found.
  # the error could be more specific
  # asNamespace might not be needed.
  tryCatch(
    {func <- get(paste0(period, "GripProc"), envir=asNamespace('mocapGrip'), mode='function')},
    error = function(e) {
      stop("Could not find a function to parse the data for the period ", period, sep="")
    }
  )

  # process the data with the function that was found, add it to periodData (along with warnings)
  periodData[["data"]] <- withCallingHandlers({func(data)},
                                              warning = function(w) {
                                                warns <<- append(warns,w$message)
                                                invokeRestart("muffleWarning")
                                              }
  )
  periodData[["warnings"]] <- warns

  # grab default analyses
  periodData[["analysesToRun"]] <- modelMetadata$dataSets[[period]]$defaultAnalysis

  return(periodData)
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
  periodData <- sapply(dataSets, processPeriod, data=data, USE.NAMES = TRUE, simplify = FALSE)

  if(includeFullData==TRUE){
    periodData[["fullData"]] <- data
  }

  return(periodData)
}
