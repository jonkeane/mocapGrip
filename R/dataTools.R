#' write csvs of data from a data object
#' @param data a data object to use for writing data.
#' @param namePrefix a character string to prepend to each file. Default: ""
#' @param path a character string of the path to save the data. Default: "./"
#' @param overwrite logical, should the writer overwrite files that already exist? Default: `FALSE`
#' @return None
#'
#' @export
writeCSVsFromData <- function(data, namePrefix = "", path="./", overwrite = FALSE) {
  dataNames <- names(data)
  filenamesout <- paste0(path, namePrefix, dataNames, ".csv")

  # test if file exists
  if(!overwrite & any(file.exists(filenamesout))) {
    stop("At least one of the files (", filenamesout, ") already exist. Use overwrite = TRUE to overwrite them.")
  }

  result <- mapply(function(dataSet, filenameout) {readr::write_csv(dataSet$data, filenameout)}, data, filenamesout)

  sapply(names(result), function(dataSet){
    message("Successfully wrote the ", dataSet, " data to ", result[[dataSet]])
  }, simplify = TRUE, USE.NAMES = FALSE)

  return(invisible())
}

#' display the models that are set to run for one dataSet
#'
#' \bold{Warning:} the name must match the dataSet
#'
#' @param name the name of the dataSet
#' @param dataSet a dataSet
#' @return named character vectors of models to run.
#'
displayAnalysesToRunOne <- function(name, dataSet) {
  modelsToRun <- dataSet$analysesToRun
  msg <- paste0(paste0(name, ":\n"), paste0("\t", modelsToRun, "\n", collapse=""))
  message(msg)
  names(modelsToRun) <- name
  return(modelsToRun)
}

#' display the models that are set to run for all data
#'
#' @param data a \code{data} object
#' @return named character vectors of models to run.
#'
#' @export
displayAnalysesToRun <- function(data) {
  message("Analyses set to run by dataSet\n------------------------------")

  # names without fulldata
  dataSetNames <- names(data)[names(data) != "fullData"]
  out <- sapply(dataSetNames, function(dataSet) {displayAnalysesToRunOne(dataSet, data[[dataSet]])}, simplify = TRUE, USE.NAMES = TRUE)
  return(out)
}



# determine which models are compatible with a dataSet based on what columns the dataSet has.
possibleModels <- function(dataSet, modelMd = modelMetadata) {
  varsAvail <- names(dataSet$data)
  analysesAvail <- modelMd$models$analyses
  varsCompatible <- sapply(names(analysesAvail), function(analysis){
    all(analysesAvail[[analysis]]$variablesToUse %in% varsAvail)
  },simplify = TRUE, USE.NAMES = TRUE)
  names(varsCompatible[varsCompatible])
}

promptForModelInput <- function(msg){
  resp <- readline(paste0(msg, " (separated by commas): "))

  # process the response
  selectedModels <- sapply(strsplit(resp, ","), sub, pattern="\\s+", replacement="", simplify = TRUE, USE.NAMES = FALSE)

  return(selectedModels)
}

# \bold{Warning:} the name must match the dataSet
addAnalysesToRunOne <- function(name, dataSet, modelMd = modelMetadata){
  message("Analyses set to run already for the dataSet ", appendLF = FALSE)
  # grab the old models, figure out posssible models,
  # and subset possible models that are not attached
  oldModels <- displayAnalysesToRunOne(name, dataSet)
  possModels <- possibleModels(dataSet)
  newModels <-possModels[!possModels %in% oldModels]

  # add numbers to the new models
  names(newModels) <- c(1:length(newModels))

  # prompt the user
  message("Possible additional models are:\n", appendLF = FALSE)
  message(paste0("\t", paste0(names(newModels), ") ", newModels), "\n", collapse=""))
  selectedModels <- promptForModelInput("Select models to add")

  # check the response, reprompt if there are not at least one number in each response.
  while(any(!selectedModels %in% names(newModels))) {
    message("Please select the model by number.")
    selectedModels <- promptForModelInput("Select models to add")
  }

  modelsOut <- c(oldModels, newModels[names(newModels) %in% selectedModels])
  names(modelsOut) <- NULL
  return(modelsOut)
}

#' add models to the analysesToRun list.
#'
#' @param data a \code{data} object
#' @param modelMd a \code{modelMetadata} object to retrieve analyses from, by default the package's \code{modelMetadata}
#' @return a \code{data} object
#'
#' @export
addAnalysesToRun <- function(data, modelMd = modelMetadata) {
  # names without fulldata
  dataSetNames <- names(data)[names(data) != "fullData"]

  # prompt for models
  newModels <- sapply(dataSetNames, function(dataSet) {addAnalysesToRunOne(dataSet, data[[dataSet]], modelMd)}, simplify = FALSE, USE.NAMES = TRUE)

  # add new models to data
  for(dataSet in names(newModels)) {
    data[[dataSet]]$analysesToRun <- newModels[[dataSet]]
  }

  return(data)
}


# \bold{Warning:} the name must match the dataSet
removeAnalysesToRunOne <- function(name, dataSet, modelMd = modelMetadata){
  message("Analyses set to run already for the dataSet ", appendLF = FALSE)
  # grab the old models, figure out posssible models,
  # and subset possible models that are not attached
  oldModels <- displayAnalysesToRunOne(name, dataSet)

  # add numbers to the old models
  names(oldModels) <- c(1:length(oldModels))

  # prompt the user
  message("Models to remove:\n", appendLF = FALSE)
  message(paste0("\t", paste0(names(oldModels), ") ", oldModels), "\n", collapse=""))
  selectedModels <- promptForModelInput("Select models to remove")
  if(!selectedModels %in% c("none", "None", "NONE")){
    # if the resposne is not an escape string
    # check the response, reprompt if there are not at least one number in each response.
    while(any(!{selectedModels %in% names(oldModels)})) {
      message("Please select the model by number. Type 'none' to keep all models present.")
      selectedModels <- promptForModelInput("Select models to remove")
      if(selectedModels %in% c("none", "None", "NONE")) { break }
    }
  }

  modelsOut <- oldModels[!names(oldModels) %in% selectedModels]
  names(modelsOut) <- NULL
  return(modelsOut)
}

#' add models to the analysesToRun list.
#'
#' @param data a \code{data} object
#' @param modelMd a \code{modelMetadata} object to retrieve analyses from, by default the package's \code{modelMetadata}
#' @return a \code{data} object
#'
#' @export
removeAnalysesToRun <- function(data, modelMd = modelMetadata) {
  # names without fulldata
  dataSetNames <- names(data)[names(data) != "fullData"]

  # prompt for models
  newModels <- sapply(dataSetNames, function(dataSet) {removeAnalysesToRunOne(dataSet, data[[dataSet]], modelMd)}, simplify = FALSE, USE.NAMES = TRUE)

  # add new models to data
  for(dataSet in names(newModels)) {
    # warn if there are no models left
    if(length(newModels[[dataSet]]) == 0) {warning("There are no more analysesToRun for dataSet ", dataSet, ". This means no models will be fit for it if you don't add other models.")}
    data[[dataSet]]$analysesToRun <- newModels[[dataSet]]
  }

  return(data)
}

#' check the form of data object
#'
#' @param data a data object to be checked
#' @param modelMd a modelStructure to use (to confirm that the dataSets are in the modelStructure)
#'
#' @return if the data objects form is correct, the data object
#'
#' @export
checkData <- function(data, modelMd) {
  errormsg <- "The data object supplied does not have the right form. If you've changed the data object since it was created by the readExtractedMocapData() function, something went wrong with those changes. Please try runing readExtractedMocapData() again, and using that object, if that works, try your changes one by one to see which of the changes is causing the problem."
  if( !is.list(data) ) { stop(errormsg) }

  if( length(data) < 1 ) { stop(errormsg) }

  # check that the names of data are names of dataSets in modelMd (or fullData)
  dataSetNames <- names(data)
  if(!all(dataSetNames %in% c(names(modelMd$dataSets), "fullData"))) { stop(errormsg, dataSet, "names of the dataSets") }

  for(dataSet in dataSetNames) {
    # check that the names within each dataSet are correct
    dataSetSubNames <- names(data[[dataSet]])
    if(!all(dataSetSubNames %in% c("data", "warnings", "analysesToRun", "analyses"))) {stop(errormsg, dataSet, ": names within the dataSet")  }
    for(dataSetSub in dataSetSubNames) {
      # check that the dataSetSub structures are the right type
      if(dataSetSub == "data") {
        if(!is.data.frame(data[[dataSet]][[dataSetSub]])) { stop(errormsg, dataSet, ": data") }
      }
      if(dataSetSub == "warnings") {
        if(!is.list(data[[dataSet]][[dataSetSub]])) { stop(errormsg, dataSet, ": warnings")  }
      }
      if(dataSetSub == "analysesToRun") {
        # check that these are in the modelMd?
        if(!is.character(data[[dataSet]][[dataSetSub]])) { stop(errormsg, dataSet, ": analysesToRun") }
      }
      if(dataSetSub == "analyses") {
        if(!is.list(data[[dataSet]][[dataSetSub]])) { stop(errormsg, dataSet, ": analyses")  }
      }
    }
  }

  return(data)
}
