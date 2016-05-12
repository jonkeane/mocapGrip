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
