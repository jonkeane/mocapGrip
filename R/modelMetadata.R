# data <- readExtractedMocapData(path="~/Dropbox/mocap/gripStudy/analysis/extractedData/")

#' model analysis structure, and explanation information
#'
#' This is read from modelMetadata.json, and different structures can be read by \code{\link{readModelMetadata}}
#'
#' @export
modelMetadata <- jsonlite::fromJSON(system.file("modelMetadata.json", package = "mocapGrip", mustWork = TRUE))

#' Check that modelMetadata conforms to the standard
#'
#' @param modelMd a modelMetadata object
#'
#' @return the modelMetadata object that was checked (if it passes)
checkmodelMetadata <- function(modelMd){
  # check if modelMd is a list.
  if( !is.list(modelMd) ) { stop("The model metadata is not a list.") }

  # check the variableExplaantions section.
  if( !"variableExplanations" %in% names(modelMd) ) { stop("The model metadata does not have a section of variableExplanations.") }
  if(length(modelMd$variableExplanations) < 1) {
    warning("The variableExplanations does not have any content. This is ok if the variables used in the models section already have explanations in the standard modelMetadata.")
  } else {
    variableExplanationNames <- names(modelMd$variableExplanations)
    # check each of the variable explanations
    lapply(variableExplanationNames, function(varExp){
      if(!is.character(modelMd$variableExplanations[[varExp]])) {stop("The variableExplanation '", varExp, "' is not a character.")}
      })
  }

  # check the models section
  if( !"models" %in% names(modelMd) ) { stop("The model metadata does not have a section of models.") }
  # nothing about analysis skeleton for now

  # check the analysis section
  if( !"analyses" %in% names(modelMd$models) ) { stop("The models section of that model metadata does not have a section of analyses") }
  if(length(modelMd$models$analyses) < 1) {
    warning("The analysis section of the model section of the model metadata does not have any content. This is ok if there are no new models to be added.")
  } else {
    analysisNames <- names(modelMd$models$analyses)
    lapply(analysisNames, function(analysis) {
      currAnalysis <- modelMd$models$analyses[[analysis]]
      # make sure the analysis is a list.
      if(!is.list(modelMd$models$analyses[[analysis]])){stop("The analysis ", analysis, " is not a list. Something is wrong with the specificatoins.")}
      if(!"variablesToUse" %in% names(modelMd$models$analyses[[analysis]])  ){stop("The analysis ", analysis, " does not have a variables to use section.")}
      # no checking of plotting data for now.
      # no checking that the predictors and variable explanations match
    })
  }

  if( !"modelStructures" %in% names(modelMd$models) ) { stop("The models section of that model metadata does not have a section of modelStructures (formulas)")}
  if(length(modelMd$models$modelStructures) < 1) {
    warning("The modelStructures section of the model section of the model metadata does not have any content. This is ok if there are no new model structures to be added.")
  } else {
    msNames <- names(modelMd$models$modelStructures)
    lapply(msNames, function(msName) {
      currAnalysis <- modelMd$models$modelStructures[[msName]]
      # make sure the msName is a list.
      if(!is.character(modelMd$models$modelStructures[[msName]])){stop("The modelStructure ", msName, " is not a character string. Something is wrong with the specificatoins.")}
      if(!grepl("paste0\\(.*~.*\\)", modelMd$models$modelStructures[[msName]])){stop("The modelStructure ", msName, " is not formatted correctly.")}
      # no checking of plotting data for now.
      # no checking that the predictors and variable explanations match
    })
  }

  # check the dataSets section
  if( !"dataSets" %in% names(modelMd) ) { stop("The model metadata does not have a section of dataSets.") }
  if(length(modelMd$dataSets) < 1) {
    warning("The dataSets does not have any content. This is ok if the variables used in the models section already have explanations in the standard modelMetadata.")
  } else {
    dataSetNames <- names(modelMd$dataSets)
    # check each of the variable explanations
    lapply(dataSetNames, function(dataSet){
      if(!is.list(modelMd$dataSets[[dataSet]])) {stop("The dataSet '", dataSet, "' is not a list")}
      # test the narrative section
      if(!"narrative" %in% names(modelMd$dataSets[[dataSet]])) {stop("There is no narrative section for the dataSet ", dataSet)}
       else {
         if(!"title" %in% names(modelMd$dataSets[[dataSet]]$narrative)) {
           stop("There is no title in the narrative section for the dataSet ", dataSet)
         } else {
           if(!is.character(modelMd$dataSets[[dataSet]]$narrative$title)) {stop("The title for dataSet ", dataSet, " is not a character string.")}
         }
         if(!"intro" %in% names(modelMd$dataSets[[dataSet]]$narrative)) {
           stop("There is no intro in the narrative section for the dataSet ", dataSet)
         } else {
           if(!is.character(modelMd$dataSets[[dataSet]]$narrative$intro)) {stop("The intro for dataSet ", dataSet, " is not a character string.")}
         }
       }
      # test the processing sectoin
      if(!"processing" %in% names(modelMd$dataSets[[dataSet]])) {stop("There is no processing section for the dataSet ", dataSet)}
      else {
        if(!"filterString" %in% names(modelMd$dataSets[[dataSet]]$processing)) {
          stop("There is no filterString in the processing section for the dataSet ", dataSet)
        } else {
          if(!is.character(modelMd$dataSets[[dataSet]]$processing$filterString)) {stop("The filterString for dataSet ", dataSet, " is not a character string.")}
        }
        if(!"processFunction" %in% names(modelMd$dataSets[[dataSet]]$processing)) {
          stop("There is no processFunction in the processing section for the dataSet ", dataSet)
        } else {
          if(!is.character(modelMd$dataSets[[dataSet]]$processing$processFunction)) {stop("The processFunction for dataSet ", dataSet, " is not a character string.")}
        }
        if(!"processFunctionOptions" %in% names(modelMd$dataSets[[dataSet]]$processing)) {
          stop("There is no processFunctionOptions in the processing section for the dataSet ", dataSet)
        } else {
          if(!is.list(modelMd$dataSets[[dataSet]]$processing$processFunctionOptions)) {stop("The processFunctionOptions for dataSet ", dataSet, " is not a list.")}
        }
      }
      # test the defaultAnalysis section
      if(!"defaultAnalysis" %in% names(modelMd$dataSets[[dataSet]])) {
          stop("There is no defautlAnalysis section for the dataSet ", dataSet)
      } else {
        if(!is.character(modelMd$dataSets[[dataSet]]$defaultAnalysis)) {stop("The defaultAnalysis for dataSet ", dataSet, " is not a character string.")}
        }
    })
  }

  # check the dataPreProcessing section.
  if( !"dataPreProcessing" %in% names(modelMd) ) { stop("The model metadata does not have a section of dataPreProcessing") }
  if(length(modelMd$dataPreProcessing) < 1) {
    warning("The dataPreProcessing does not have any content. This is ok if the variables used in the models section already have explanations in the standard modelMetadata.")
  } else {
    # check each of the variable explanations
    lapply(modelMd$dataPreProcessing, function(varExp){
      if(!is.character(modelMd$dataPreProcessing)) {stop("The dataPreProcessing is not a character.")}
    })
  }

  # check the dataPreProcessing section.
  if( !"dataPreProcessing" %in% names(modelMd) ) { stop("The model metadata does not have a section of dataPreProcessing") }
  if(length(modelMd$dataPreProcessing) < 1) {
    warning("The dataPreProcessing does not have any content. This is ok if the variables used in the models section already have explanations in the standard modelMetadata.")
  } else {
    # check each of the variable explanations
    lapply(modelMd$dataPreProcessing, function(varExp){
      if(!is.character(modelMd$dataPreProcessing)) {stop("The dataPreProcessing is not a character.")}
    })
  }

  # check that there are no other names
  if( any(! names(modelMd) %in% c("variableExplanations", "models", "dataSets", "dataPreProcessing", "dataSetPostProcessing")) ) { stop("The model metadata has more sections than just variableExplanations, models, dataSets. It has: ", names(modelMd)) }


  modelMd <- checkVariablesToUse(modelMd)

  return(modelMd)
}

#' reads model metadata from a specially formated json file
#'
#' This is useful if you want to use different types of models/analyses or dataSets. Pass the modelMetadata object that is returned form this object to functions that take it as an argument (e.g. \code{\link{readExtractedMocapData}}, \code{\link{makeReport}}) to override the package defaults.
#' Additionally, it preforms checks to make sure that the modelMetadata conforms to expected structure.
#'
#' @param file a path to a json file that includes modelMetadata
#'
#' @return a modelMetadata object
#'
#' @export
readModelMetadata <- function(file){
  if(tools::file_ext(file) != "json") {warning("The file specified (", file, ") does not end in .json. This is likely a typo, however this will not stop the reading of the file.")}

  modelMetadataNew <- jsonlite::fromJSON(file)
  checkmodelMetadata(modelMetadataNew)
}

#' writes model metadata from a specially formated json file
#'
#' Writes the modelMetadata object to a json file. This is useful if you want to change the modelMetadata, but don't know what the structure should look like. As an additional precaution, the modelMetadata that is given is checked to make sure that it is valid before it is written.
#'
#' @param modelMd a modelMetadata object
#' @param path a path to write a json file out
#' @param overwrite if the path exists, should it be overwritten?
#'
#' @return nothing
#'
#' @export
writeModelMetadata <- function(modelMd, path, overwrite = FALSE){
  if(tools::file_ext(path) != "json") {warning("The path specified (", path, ") does not end in .json. This is likely a typo, however this will not stop the writting of the file.")}
  if(!overwrite & file.exists(path) ) {stop("The path specified (", path, ") already exits. If you want to overwrite, please pass the argument overwrite = TRUE")}
  modelMdToWrite <- NULL
  modelMdToWrite <- checkmodelMetadata(modelMd)
  if(!is.null(modelMdToWrite)){
    jsonOut <- jsonlite::prettify(jsonlite::toJSON(modelMdToWrite))
    fileConn<-file(path)
    writeLines(jsonOut, fileConn)
    close(fileConn)
  } else {
    stop("The modelMetadata ",  deparse(substitute(modelMd)), " was not well formed, so no modelMetadata was written.")
  }
}


#' Check that modelMetadata has all the variables and explanations that are needed
#'
#' @param modelMd a modelMetadata object
#'
#' @return the modelMetadata object that was checked (if it passes)
checkVariablesToUse <- function(modelMd){
  varExp <- names(modelMd$variableExplanations)
  sapply(names(modelMd$models$analyses), function(analysis){
    sapply(modelMd$models$analyses[[analysis]]$variablesToUse, function(var){
      if(! var %in% varExp) {
        stop("The variable ", var, " in the analysis ", analysis, "does not also have a variable explanation. Please add it to the modelMetadata (or modelMetadata.json file)")
      }
    })
  }, simplify = TRUE, USE.NAMES = TRUE)


  return(modelMd)
}
