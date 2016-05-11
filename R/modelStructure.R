#' modelStructure
#'
#' @export
modelStructure <- jsonlite::fromJSON(system.file("modelStructure.json", package = "mocapGrip", mustWork = TRUE))

checkModelStructure <- function(modelStructure){
  # check if modelStructure is a list.
  if( !is.list(modelStructure) ) { stop("The model structure is not a list.") }

  # check the variableExplaantions section.
  if( !"variableExplanations" %in% names(modelStructure) ) { stop("The model structure does not have a section of variableExplanations.") }
  if(length(modelStructure$variableExplanations) < 1) {
    warning("The variableExplanations does not have any content. This is ok if the variables used in the models section already have explanations in the standard modelStructure.")
  } else {
    variableExplanationNames <- names(modelStructure$variableExplanations)
    # check each of the variable explanations
    lapply(variableExplanationNames, function(varExp){
      if(!is.character(modelStructure$variableExplanations[[varExp]])) {stop("The variableExplanation '", varExp, "' is not a character.")}
      })
  }

  # check the models section
  if( !"models" %in% names(modelStructure) ) { stop("The model structure does not have a section of models.") }
  # nothing about analysis skeleton for now

  # check the analysis section
  if( !"analyses" %in% names(modelStructure$models) ) { stop("The models section of that model structure does not have a section of analyses") }
  if(length(modelStructure$models$analyses) < 1) {
    warning("The analysis section of the model section of the model structure does not have any content. This is ok if there are no new models to be added.")
  } else {
    analysisNames <- names(modelStructure$models$analyses)
    lapply(analysisNames, function(analysis) {
      currAnalysis <- modelStructure$models$analyses[[analysis]]
      # make sure the analysis is a list.
      if(!is.list(modelStructure$models$analyses[[analysis]])){stop("The analysis ", analysis, " is not a list. Something is wrong with the specificatoins.")}
      if(!"variablesToUse" %in% names(modelStructure$models$analyses[[analysis]])  ){stop("The analysis ", analysis, " does not have a variables to use section.")}
      # no checking of plotting data for now.
      # no checking that the predictors and variable explanations match
    })
  }

  if( !"modelStructures" %in% names(modelStructure$models) ) { stop("The models section of that model structure does not have a section of modelStructures (formulas)")}
  if(length(modelStructure$models$modelStructures) < 1) {
    warning("The modelStructures section of the model section of the model structure does not have any content. This is ok if there are no new model structures to be added.")
  } else {
    msNames <- names(modelStructure$models$modelStructures)
    lapply(msNames, function(msName) {
      currAnalysis <- modelStructure$models$modelStructures[[msName]]
      # make sure the msName is a list.
      if(!is.character(modelStructure$models$modelStructures[[msName]])){stop("The modelStructure ", msName, " is not a character string. Something is wrong with the specificatoins.")}
      if(!grepl("paste0\\(.*~.*\\)", modelStructure$models$modelStructures[[msName]])){stop("The modelStructure ", msName, " is not formatted correctly.")}
      # no checking of plotting data for now.
      # no checking that the predictors and variable explanations match
    })
  }

  # check the dataSets section
  if( !"dataSets" %in% names(modelStructure) ) { stop("The model structure does not have a section of dataSets.") }
  if(length(modelStructure$dataSets) < 1) {
    warning("The dataSets does not have any content. This is ok if the variables used in the models section already have explanations in the standard modelStructure.")
  } else {
    dataSetNames <- names(modelStructure$dataSets)
    # check each of the variable explanations
    lapply(dataSetNames, function(dataSet){
      if(!is.list(modelStructure$dataSets[[dataSet]])) {stop("The dataSet '", dataSet, "' is not a list")}
      if(!"narrative" %in% names(modelStructure$dataSets[[dataSet]])) {stop("There is no narrative section for the dataSet ", dataSet)}
       else {
         if(!"title" %in% names(modelStructure$dataSets[[dataSet]]$narrative)) {
           stop("There is no title in the narrative section for the dataSet ", dataSet)
         } else {
           if(!is.character(modelStructure$dataSets[[dataSet]]$narrative$title)) {stop("The title for dataSet ", dataSet, " is not a character string.")}
         }
         if(!"intro" %in% names(modelStructure$dataSets[[dataSet]]$narrative)) {
           stop("There is no intro in the narrative section for the dataSet ", dataSet)
         } else {
           if(!is.character(modelStructure$dataSets[[dataSet]]$narrative$intro)) {stop("The intro for dataSet ", dataSet, " is not a character string.")}
         }
        }
      if(!"defaultAnalysis" %in% names(modelStructure$dataSets[[dataSet]])) {
          stop("There is no defautlAnalysis section for the dataSet ", dataSet)
      } else {
        if(!is.character(modelStructure$dataSets[[dataSet]]$defaultAnalysis)) {stop("The defaultAnalysis for dataSet ", dataSet, " is not a character string.")}
        }
    })
  }
  # check that there are no other names
  if( any(! names(modelStructure) %in% c("variableExplanations", "models", "dataSets")) ) { stop("The model structure has more sections than just variableExplanations, models, dataSets. It has: ", names(modelStructure)) }


}

# formulaGood <- "paste0(outcome, '~', predictor1, '*', predictor2, '+', '(', '1+', predictor1, '*', predictor2, '|', grouping1, ')')"
# checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
#                          "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
#                                          "modelStructures" = list("foo" = formulaGood)),
#                          "dataSets" = list("foo" = list("narrative" = list("title" = character(),
#                                                                            "intro" = character()),
#                                                         "defaultAnalysis" = 1))))
