process <- function(file, conditionCodesFile, verbose=FALSE){
  # read in csvs as strings including NA (which indicates occlusion) as a string (and empty as a true NA)
  data <- read.csv(file=file, na.strings = "", stringsAsFactors=FALSE)
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

  condCodes <- read.csv(conditionCodesFile)

  # change to dplyr inner_join?
  meltedData <- plyr::join(meltedData, condCodes)

  meltedData
}

# grab only grip and time data
importCols <- function(data){
  # currently results in note, because of the nonpublic import of one_of (which is special for dplyr)
  data %>% dplyr::select(-dplyr:::one_of(c("times", "grip")))
}


# processing function for maximum grip period
maxGripProc <- function(data) {
  # grip <- subset(data, period == "GRIP")
  grip <- data[data$period == "GRIP", ]
  if(nrow(grip) == 0 ){
    warning(paste("There was no GRIP period found for: obsisSubj:", unique(data$obsisSubj), "obsisTrail:", unique(data$obsisTrial), "condition", unique(data$condition), "trial type:", unique(data$type), "The following periods were found:", paste(unique(data$period), collapse=", "),  sep =" "))
    return(data.frame())
  }
  if(sum(is.na(grip$grip))/nrow(grip) > 0.05 ){
    warning(paste("There is ", as.character(round(sum(is.na(grip$grip))/nrow(grip)*100, digits=4)), "% occlusion for: obsisSubj: ", unique(data$obsisSubj), " obsisTrail: ", unique(data$obsisTrial), " condition ", unique(data$condition), " trial type: ", unique(data$type),  sep =""))
    return(data.frame())
  }
  maxTime <- max(grip$times, na.rm = TRUE)
  # add logic to catch multiple maximums
  maxGripRow <- grip[which.max(grip$grip),]
  return(cbind(data.frame(duration=maxTime, maxGrip=maxGripRow$grip, maxGripTime=maxGripRow$times, maxGripTimeRel=maxGripRow$times/maxTime), importCols(maxGripRow)))
}

# processing function for steady period
steadyProc <- function(data) {
  # subData <- subset(data, period == "STEADY")
  subData <- data[data$period == "STEADY",]

  if(nrow(subData) == 0 ){
    warning(paste("There was no STEADY period found for: obsisSubj:", unique(data$obsisSubj), "obsisTrail:", unique(data$obsisTrial), "condition", unique(data$condition), "trial type:", unique(data$type), "The following periods were found:", paste(unique(data$period), collapse=", "),  sep =" "))
    return(data.frame())
  }
  if(sum(is.na(subData$grip))/nrow(subData) > 0.05 ){
    warning(paste("There is ", as.character(round(sum(is.na(subData$grip))/nrow(subData)*100), digits=4), "% occlusion for: obsisSubj: ", unique(data$obsisSubj), " obsisTrail: ", unique(data$obsisTrial), " condition ", unique(data$condition), " trial type: ", unique(data$type),  sep =""))
    return(data.frame())
  }

  maxTime <- max(subData$times, na.rm=TRUE)
  meanGrip <- mean(subData$grip, na.rm=TRUE)
  medianGrip <- median(subData$grip, na.rm=TRUE)
  cbind(data.frame(duration=maxTime, meanGrip=meanGrip, medianGrip=medianGrip), importCols(subData[1,]))
}


# processing function for maximum release period
maxReleaseProc <- function(data) {
  # grip <- subset(data, period == "RELEASE")
  grip <- data[data$period == "RELEASE", ]
  if(nrow(grip) == 0 ){
    warning(paste("There was no RELEASE period found for: obsisSubj:", unique(data$obsisSubj), "obsisTrail:", unique(data$obsisTrial), "condition", unique(data$condition), "trial type:", unique(data$type), "The following periods were found:", paste(unique(data$period), collapse=", "),  sep =" "))
    return(data.frame())
  }
  if(sum(is.na(grip$grip))/nrow(grip) > 0.05 ){
    warning(paste("There is ", as.character(round(sum(is.na(grip$grip))/nrow(grip)*100, digits=4)), "% occlusion for: obsisSubj: ", unique(data$obsisSubj), " obsisTrail: ", unique(data$obsisTrial), " condition ", unique(data$condition), " trial type: ", unique(data$type),  sep =""))
    return(data.frame())
  }
  maxTime <- max(grip$times, na.rm = TRUE)
  # add logic to catch multiple maximums
  maxGripRow <- grip[which.max(grip$grip),]
  return(cbind(data.frame(duration=maxTime, maxGrip=maxGripRow$grip, maxGripTime=maxGripRow$times, maxGripTimeRel=maxGripRow$times/maxTime), importCols(maxGripRow)))
}


# processing function for movement period
moveProc <- function(data) {
  # subData <- subset(data, period == "MOVEMENT")
  subData <- data[data$period == "MOVEMENT", ]

  if(nrow(subData) == 0 ){
    warning(paste("There was no MOVEMENT period found for: obsisSubj:", unique(data$obsisSubj), "obsisTrail:", unique(data$obsisTrial), "condition", unique(data$condition), "trial type:", unique(data$type), "The following periods were found:", paste(unique(data$period), collapse=", "),  sep =" "))
    return(data.frame())
  }
  if(sum(is.na(subData$grip))/nrow(subData) > 0.05 ){
    warning(paste("There is ", as.character(round(sum(is.na(subData$grip))/nrow(subData)*100), digits=4), "% occlusion for: obsisSubj: ", unique(data$obsisSubj), " obsisTrail: ", unique(data$obsisTrial), " condition ", unique(data$condition), " trial type: ", unique(data$type),  sep =""))
    return(data.frame())
  }

  maxTime <- max(subData$times, na.rm=TRUE)
  meanGrip <- mean(subData$grip, na.rm=TRUE)
  medianGrip <- median(subData$grip, na.rm=TRUE)
  cbind(data.frame(duration=maxTime, meanGrip=meanGrip, medianGrip=medianGrip), importCols(subData[1,]))
}



actionGripProc <-function(data) {
  # extract the maxmium grip during the grip portion of all action trials
  data %>% dplyr::filter_("type"=="ACTION") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(maxGripProc("."))
}

releaseGripProc <-function(data) {
  # extract the maxmium grip during the grip portion of all action trials
  data %>% dplyr::filter_("type"=="ACTION") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(maxReleaseProc("."))
}

estSteadyProc <-function(data) {
  # extract the mean and median grip during the steady portion of all estimate trials
  data %>% dplyr::filter_("type"=="ESTIMATION") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(steadyProc("."))
}

estMaxGripProc <-function(data) {
  # extract the maxmium grip during the grip portion of all estimate trials
  data %>% dplyr::filter_("type"=="ESTIMATION") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(maxGripProc("."))
}

gestMoveGripProc <-function(data) {
  actionSideDF <- data %>% dplyr::filter_("type"=="ACTION") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::summarise_(actionSide=unique("side"))
  # extract the mean and median grip during the move portion of all gesture trials
  merge(data %>% dplyr::filter_("type"=="GESTURE") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(moveProc(".")), actionSideDF, all.y=FALSE)
}

gestMaxGripProc <-function(data) {
  actionSideDF <- data %>% dplyr::filter_("type"=="ACTION") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::summarise_(actionSide=unique("side"))
  # extract the maxmium grip during the grip portion of all gesture trials
  merge(data %>% dplyr::filter_("type"=="GESTURE") %>% dplyr::group_by_(c("obsisSubj","obsisTrial","condition")) %>% dplyr::do_(maxGripProc(".")), actionSideDF, all.y=FALSE)
}


#' Read extracted motion capture data
#'
#' Reads in extracted motion capture data from a directory.
#'
#' @param path Directory containing motion catpure csv files that were extracted with the \code{\link{extractAnnotations}} function.
#' @return Not sure yet.
#'
#' @export
readExportedMocapData <- function(path){
  # to be added to main function for oparsing data
  files <- list.files(path, recursive = TRUE, pattern = NULL, full.names=TRUE)

  data <- plyr::ldply(files, process, conditionCodesFile="GRIPMLstimuli.csv", verbose=FALSE)

  data$stick <- factor(as.character(data$stick), levels = c("five", "seven", "nine", "eleven"))

  data$stickcmScaled <- data$stickcm - 8

  # action grip
  actionData <- actionGripProc(data)
  # write.csv(file="action.csv", actionData)

  # release data
  releaseData <- releaseGripProc(data)
  # write.csv(file="action.csv", actionData)

  # estimation steady
  estimationData <- estSteadyProc(data)
  # write.csv(file="estimation.csv", estimationData)

  # estimation max grip
  estimationMaxGripData <- estMaxGripProc(data)
  # write.csv(file="estimationGrip.csv", estimationMaxGripData)

  # gesture max grip
  # gestureMaxGripData <- gestMaxGripProc(data)
  # write.csv(file="gestureGrip.csv", gestureMaxGripData)

  #gesture movement
  # gestMoveData <- gestMoveGripProc(data)
  # write.csv(file="gestureMove.csv", gestMoveData)

  return(list("action"=actionData, "estimation"=estimationData, "maxGripFromEstimation" = estimationMaxGripData))
}