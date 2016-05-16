# data <- readExtractedMocapData(path="~/Dropbox/mocap/gripStudy/analysis/extractedData/")

#' model analysis structure, and explanation information
#'
#' This is read from modelMetadata.json, and different structures can be read by \code{readmodelMetadata()}
#'
#' @export
modelMetadata <- jsonlite::fromJSON(system.file("modelMetadata.json", package = "mocapGrip", mustWork = TRUE))

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




  # check that there are no other names
  if( any(! names(modelMd) %in% c("variableExplanations", "models", "dataSets", "dataPreProcessing")) ) { stop("The model metadata has more sections than just variableExplanations, models, dataSets. It has: ", names(modelMd)) }

  return(modelMd)
}

