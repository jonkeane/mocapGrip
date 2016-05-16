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
  if (nrow(data) == 0) {
    warning(
      paste(
        "There was no period found for obsisSubj:",
        unique(data$obsisSubj),
        "obsisTrail:",
        unique(data$obsisTrial),
        "condition",
        unique(data$condition),
        "trial type:",
        unique(data$type),
        "The following periods were found:",
        paste(unique(data$period), collapse = ", "),
        sep = " "
      )
    )
    return(data.frame())
  }
  if (sum(is.na(data$grip)) / nrow(data) > percOcclusion) {
    warning(
      paste(
        "There is ",
        as.character(round(sum(
          is.na(data$grip)
        ) / nrow(data) * 100, digits = 4)),
        "% occlusion for obsisSubj: ",
        unique(data$obsisSubj),
        " obsisTrail: ",
        unique(data$obsisTrial),
        " condition: ",
        unique(data$condition),
        " period: ",
        unique(data$period),
        " trial type: ",
        unique(data$type),
        sep = ""
      )
    )
    return(data.frame())
  }
  maxTime <- max(data$times, na.rm = TRUE)
  # add logic to catch multiple maximums
  maxGripRow <- data[which.max(data$grip), ]

  return(cbind(
    data.frame(
      duration = maxTime,
      maxGrip = maxGripRow$grip,
      maxGripTime = maxGripRow$times,
      maxGripTimeRel = maxGripRow$times / maxTime
    ),
    importCols(maxGripRow)
  ))
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
  if (nrow(data) == 0) {
    warning(
      paste(
        "There was no period found for obsisSubj:",
        unique(data$obsisSubj),
        "obsisTrail:",
        unique(data$obsisTrial),
        "condition",
        unique(data$condition),
        "trial type:",
        unique(data$type),
        "The following periods were found:",
        paste(unique(data$period), collapse = ", "),
        sep = " "
      )
    )
    return(data.frame())
  }
  if (sum(is.na(data$grip)) / nrow(data) > percOcclusion) {
    warning(
      paste(
        "There is ",
        as.character(round(sum(
          is.na(data$grip)
        ) / nrow(data) * 100), digits = 4),
        "% occlusion for obsisSubj: ",
        unique(data$obsisSubj),
        " obsisTrail: ",
        unique(data$obsisTrial),
        " condition: ",
        unique(data$condition),
        " period: ",
        unique(data$period),
        " trial type: ",
        unique(data$type),
        sep = ""
      )
    )
    return(data.frame())
  }

  maxTime <- max(data$times, na.rm = TRUE)
  meanGrip <- mean(data$grip, na.rm = TRUE)
  medianGrip <- stats::median(data$grip, na.rm = TRUE)

  return(cbind(
    data.frame(
      duration = maxTime,
      meanGrip = meanGrip,
      medianGrip = medianGrip
    ),
    importCols(data[1, ])
  ))
}



#' Processing function for finding means or medians of a subset of the period
#'
#' \code{meanMedianSubsetFinder()} takes the dataSet and extracts the mean and median of grip for a subset of the dataSet.
#' This function should only be used in \code{\link{processDataSet}}, because that has the code to group all
#'  of the data into the right groups based on subject, session, trial, condition, period, etc. This should be
#'  included in the processing section of \link{modelMetadata} called \code{processFunction}
#'
#' @param data the data to process.
#' @param percOcclusion the percentage of occlusion that is acceptable (this is the upper bound, in percent.) Default: \code{0.05}, or only trials where there is less than 5\% of occlusion are processed.
#' @param start when to start the subset, either in percentage (default) or milliseconds
#' @param stop when to stop the subset, either in percentage (default) or milliseconds
#' @param timeType a character, either "percentage" if the times given should be used as percentages (this is the default); or "msecs" if the times given should be used as miliseconds
#'
#' @return a dataframe with the data that has been processed, one line per observation
#'
meanMedianSubsetFinder <- function(data, percOcclusion = 0.05, start, stop, timeType = "msecs") {
  if (nrow(data) == 0) {
    warning(
      paste(
        "There was no period found for obsisSubj:",
        unique(data$obsisSubj),
        "obsisTrail:",
        unique(data$obsisTrial),
        "condition",
        unique(data$condition),
        "trial type:",
        unique(data$type),
        "The following periods were found:",
        paste(unique(data$period), collapse = ", "),
        sep = " "
      )
    )
    return(data.frame())
  }
  if (sum(is.na(data$grip)) / nrow(data) > percOcclusion) {
    warning(
      paste(
        "There is ",
        as.character(round(sum(
          is.na(data$grip)
        ) / nrow(data) * 100), digits = 4),
        "% occlusion for obsisSubj: ",
        unique(data$obsisSubj),
        " obsisTrail: ",
        unique(data$obsisTrial),
        " condition: ",
        unique(data$condition),
        " period: ",
        unique(data$period),
        " trial type: ",
        unique(data$type),
        sep = ""
      )
    )
    return(data.frame())
  }
  # error if the start is higher than the stop.
  if(start>stop) { stop("The value given for start is higher than the value given for stop. They must be reversed.") }

  maxTime <- max(data$times, na.rm = TRUE)
  if(timeType == "percent"){
    # check and warn if times are 0<start,stop<1
    if(start>1|start<0) {stop("The start time given (", start, ") is greater than one or less than zero. For percentages, it must be within 0 and 1")}
    if(stop>1|stop<0) {stop("The stop time given (", start, ") is greater than one or less than zero. For percentages, it must be within 0 and 1")}

    # change percentages to times
    startTime <- maxTime*start
    stopTime <- maxTime*stop

    # subset the data, grab the mean, median, etc.
    subData <- data %>% dplyr::filter_(paste0("times >= ",startTime, " & times <= ",stopTime))
    meanGrip <- mean(data$grip, na.rm = TRUE)
    medianGrip <- stats::median(data$grip, na.rm = TRUE)
    durTime <- stopTime-startTime

  } else if(timeType == "msecs"){
    # error or warn if the start or stop time is less than zero, or longer than the duration of the period
    if(start<0) {stop("The start time given (", start, ") is less than 0. It must be greater than or equal to 0 milliseconds.")}
    if(start>maxTime) {warning("The start time given (", start, ") is greater than the duration of the period. No data was extracted from ",
                               unique(data$obsisSubj),
                               " obsisTrail: ",
                               unique(data$obsisTrial),
                               " condition: ",
                               unique(data$condition),
                               " trial type:" ,
                               unique(data$type),
                               " The following periods were found: ",
                               paste(unique(data$period), collapse = ", ")
    )}
    if(stop<0) {stop("The stop time given (", stop, ") is less than 0. It must be greater than or equal to 0 milliseconds.")}
    if(stop>maxTime) {
      warning("The stop time given (", stop, ") is greater than the duration of the period. Everything until the end was extracted, but this is not the full period that was specified. ",
                              unique(data$obsisSubj),
                              " obsisTrail: ",
                              unique(data$obsisTrial),
                              " condition: ",
                              unique(data$condition),
                              " trial type:" ,
                              unique(data$type),
                              " The following periods were found: ",
                              paste(unique(data$period), collapse = ", ")
      )
      stop <- maxTime
      }
    # subset the data, grab the mean, median, etc.
    subData <- data %>% dplyr::filter_(paste0("times >= ",start, " & times <= ",stop))
    meanGrip <- mean(subData$grip, na.rm = TRUE)
    medianGrip <- stats::median(subData$grip, na.rm = TRUE)
    durTime <- stop-start

  } else {
    stop("The timeType given (", timeType,"). Is not recognized. Please use either 'percent' or 'msecs'.")
  }

  return(cbind(
    data.frame(
      duration = maxTime,
      durationOfSubset = durTime,
      meanGrip = meanGrip,
      medianGrip = medianGrip
    ),
    importCols(data[1, ])
  ))
}





#' Processing function for finding means or medians within a specific band at the top of the range of a grip
#'
#' \code{ceilingFinder()} takes the dataSet and extracts the mean and median of grip only for the period during which the grip is within a band at the top of the range of the grip measurement for the whole period. The range can be specified as either a percentage of the range or in millimeters.
#' This function should only be used in \code{\link{processDataSet}}, because that has the code to group all
#'  of the data into the right groups based on subject, session, trial, condition, period, etc. This should be
#'  included in the processing section of \link{modelMetadata} called \code{processFunction}
#'
#' @param data the data to process.
#' @param percOcclusion the percentage of occlusion that is acceptable (this is the upper bound, in percent.) Default: \code{0.05}, or only trials where there is less than 5\% of occlusion are processed.
#' @param bandWidth the width of the band at the top of the range
#' @param bandType a character, either "percentage" if the band given should be used as a percentage of the range (this is the default); or "mm" if the band given should be used as millimeters
#'
#' @return a dataframe with the data that has been processed, one line per observation
#'
ceilingFinder <- function(data, percOcclusion = 0.05, bandWidth, bandType = "percent") {
  if (nrow(data) == 0) {
    warning(
      paste(
        "There was no period found for obsisSubj:",
        unique(data$obsisSubj),
        "obsisTrail:",
        unique(data$obsisTrial),
        "condition",
        unique(data$condition),
        "trial type:",
        unique(data$type),
        "The following periods were found:",
        paste(unique(data$period), collapse = ", "),
        sep = " "
      )
    )
    return(data.frame())
  }
  if (sum(is.na(data$grip)) / nrow(data) > percOcclusion) {
    warning(
      paste(
        "There is ",
        as.character(round(sum(
          is.na(data$grip)
        ) / nrow(data) * 100), digits = 4),
        "% occlusion for obsisSubj: ",
        unique(data$obsisSubj),
        " obsisTrail: ",
        unique(data$obsisTrial),
        " condition: ",
        unique(data$condition),
        " period: ",
        unique(data$period),
        " trial type: ",
        unique(data$type),
        sep = ""
      )
    )
    return(data.frame())
  }

  maxTime <- max(data$times, na.rm = TRUE)
  maxGrip <- max(data$grip, na.rm = TRUE)
  minGrip <- min(data$grip, na.rm = TRUE)
  rangeGrip <- maxGrip-minGrip

  if(bandType == "percent"){
    # check and warn if times are 0<bandWidth,stop<1
    if(bandWidth>1|bandWidth<0) {stop("The bandWidth given (", bandWidth, ") is greater than one or less than zero. For percentages, it must be within 0 and 1")}
    bandFloor <- maxGrip-(rangeGrip*bandWidth)
  }  else if(bandType == "mm"){
    bandFloor <- maxGrip-bandWidth
    if(bandFloor <= minGrip) {
      warning("The bandwidth given (", bandWidth, ") is greater than the range of grip for the period. This will be no different than an overall mean.",
              unique(data$obsisSubj),
              " obsisTrail: ",
              unique(data$obsisTrial),
              " condition: ",
              unique(data$condition),
              " trial type:" ,
              unique(data$type),
              " The following periods were found: ",
              paste(unique(data$period), collapse = ", ")
      )
      bandFloor <- minGrip
    }
  } else {
    stop("The bandType given (", bandType,"). Is not recognized. Please use either 'percent' or 'mm'.")
  }


  # subset the data, grab the mean, median, etc.
  data$inBand <- data$grip >= bandFloor

  # get the mean duration (this assumes each observation is ~equally spaced apart)
  times <- data$times
  meanDur<-mean(diff(times))

  subData <- data[data$inBand,]

  meanGrip <- mean(subData$grip, na.rm = TRUE)
  medianGrip <- stats::median(subData$grip, na.rm = TRUE)
  durInBand <- sum(data$inBand*meanDur)

  return(cbind(
    data.frame(
      duration = maxTime,
      durInBand = durInBand,
      meanGrip = meanGrip,
      medianGrip = medianGrip,
      gripRange = rangeGrip,
      maxGrip = maxGrip,
      bandFloor = bandFloor
    ),
    importCols(data[1, ])
  ))
}
