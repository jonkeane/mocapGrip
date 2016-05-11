# data <- readExtractedMocapData(path="~/Dropbox/mocap/gripStudy/analysis/extractedData/")

#' model analysis structure, and explanation information
#'
#' This is read from modelMetadata.json, and different structures can be read by \code{readmodelMetadata()}
#'
#' @export
modelMetadata <- jsonlite::fromJSON(system.file("modelMetadata.json", package = "mocapGrip", mustWork = TRUE))

checkmodelMetadata <- function(modelMetadata){
  # check if modelMetadata is a list.
  if( !is.list(modelMetadata) ) { stop("The model metadata is not a list.") }

  # check the variableExplaantions section.
  if( !"variableExplanations" %in% names(modelMetadata) ) { stop("The model metadata does not have a section of variableExplanations.") }
  if(length(modelMetadata$variableExplanations) < 1) {
    warning("The variableExplanations does not have any content. This is ok if the variables used in the models section already have explanations in the standard modelMetadata.")
  } else {
    variableExplanationNames <- names(modelMetadata$variableExplanations)
    # check each of the variable explanations
    lapply(variableExplanationNames, function(varExp){
      if(!is.character(modelMetadata$variableExplanations[[varExp]])) {stop("The variableExplanation '", varExp, "' is not a character.")}
      })
  }

  # check the models section
  if( !"models" %in% names(modelMetadata) ) { stop("The model metadata does not have a section of models.") }
  # nothing about analysis skeleton for now

  # check the analysis section
  if( !"analyses" %in% names(modelMetadata$models) ) { stop("The models section of that model metadata does not have a section of analyses") }
  if(length(modelMetadata$models$analyses) < 1) {
    warning("The analysis section of the model section of the model metadata does not have any content. This is ok if there are no new models to be added.")
  } else {
    analysisNames <- names(modelMetadata$models$analyses)
    lapply(analysisNames, function(analysis) {
      currAnalysis <- modelMetadata$models$analyses[[analysis]]
      # make sure the analysis is a list.
      if(!is.list(modelMetadata$models$analyses[[analysis]])){stop("The analysis ", analysis, " is not a list. Something is wrong with the specificatoins.")}
      if(!"variablesToUse" %in% names(modelMetadata$models$analyses[[analysis]])  ){stop("The analysis ", analysis, " does not have a variables to use section.")}
      # no checking of plotting data for now.
      # no checking that the predictors and variable explanations match
    })
  }

  if( !"modelStructures" %in% names(modelMetadata$models) ) { stop("The models section of that model metadata does not have a section of modelStructures (formulas)")}
  if(length(modelMetadata$models$modelStructures) < 1) {
    warning("The modelStructures section of the model section of the model metadata does not have any content. This is ok if there are no new model structures to be added.")
  } else {
    msNames <- names(modelMetadata$models$modelStructures)
    lapply(msNames, function(msName) {
      currAnalysis <- modelMetadata$models$modelStructures[[msName]]
      # make sure the msName is a list.
      if(!is.character(modelMetadata$models$modelStructures[[msName]])){stop("The modelStructure ", msName, " is not a character string. Something is wrong with the specificatoins.")}
      if(!grepl("paste0\\(.*~.*\\)", modelMetadata$models$modelStructures[[msName]])){stop("The modelStructure ", msName, " is not formatted correctly.")}
      # no checking of plotting data for now.
      # no checking that the predictors and variable explanations match
    })
  }

  # check the dataSets section
  if( !"dataSets" %in% names(modelMetadata) ) { stop("The model metadata does not have a section of dataSets.") }
  if(length(modelMetadata$dataSets) < 1) {
    warning("The dataSets does not have any content. This is ok if the variables used in the models section already have explanations in the standard modelMetadata.")
  } else {
    dataSetNames <- names(modelMetadata$dataSets)
    # check each of the variable explanations
    lapply(dataSetNames, function(dataSet){
      if(!is.list(modelMetadata$dataSets[[dataSet]])) {stop("The dataSet '", dataSet, "' is not a list")}
      if(!"narrative" %in% names(modelMetadata$dataSets[[dataSet]])) {stop("There is no narrative section for the dataSet ", dataSet)}
       else {
         if(!"title" %in% names(modelMetadata$dataSets[[dataSet]]$narrative)) {
           stop("There is no title in the narrative section for the dataSet ", dataSet)
         } else {
           if(!is.character(modelMetadata$dataSets[[dataSet]]$narrative$title)) {stop("The title for dataSet ", dataSet, " is not a character string.")}
         }
         if(!"intro" %in% names(modelMetadata$dataSets[[dataSet]]$narrative)) {
           stop("There is no intro in the narrative section for the dataSet ", dataSet)
         } else {
           if(!is.character(modelMetadata$dataSets[[dataSet]]$narrative$intro)) {stop("The intro for dataSet ", dataSet, " is not a character string.")}
         }
        }
      if(!"defaultAnalysis" %in% names(modelMetadata$dataSets[[dataSet]])) {
          stop("There is no defautlAnalysis section for the dataSet ", dataSet)
      } else {
        if(!is.character(modelMetadata$dataSets[[dataSet]]$defaultAnalysis)) {stop("The defaultAnalysis for dataSet ", dataSet, " is not a character string.")}
        }
    })
  }
  # check that there are no other names
  if( any(! names(modelMetadata) %in% c("variableExplanations", "models", "dataSets")) ) { stop("The model metadata has more sections than just variableExplanations, models, dataSets. It has: ", names(modelMetadata)) }

  return(modelMetadata)
}

