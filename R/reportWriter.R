# add kinds of models?
# export(!)
makeReport <- function(data){
  # parse data?

  # fit models from analyze
  # models <- fitLMER(eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"), data=pureReplication$action$data)

  # pick the best model (add warnings here?)

  # read/write template

  # render
  # change path

}


# takes an analysis specification and (all of the) modeled data and then returns a (named) list of replacements to be used by replaceText()
formatGatherReplacements = function(analysis, modeledData) {
  # grab variables from the analyses structure
  outcome <- modelStructure$models$analyses[[analysis]]$variablesToUse$outcome
  predictor1 <- modelStructure$models$analyses[[analysis]]$variablesToUse$predictor1
  predictor2 <- modelStructure$models$analyses[[analysis]]$variablesToUse$predictor2
  grouping1 <- modelStructure$models$analyses[[analysis]]$variablesToUse$grouping1

  # check if there is an interaction in the predictors.
  if(names(modeledData[[analysis]]$models$bestModel) %in% c("interactionInPredAndGroup", "interactionInPred")){
    interaction <- modelStructure$variableExplanations[[paste(predictor1, predictor2, sep = "X")]]
  } else {
    interaction <- NULL
  }

  # make and format $predictorVariables
  predictorVariables <- paste0("\n* ",
                               c(modelStructure$variableExplanations[[predictor1]],
                                 modelStructure$variableExplanations[[predictor2]],
                                 interaction),
                               collapse = "")


  # add interaction text if  the selected model is $includeInteractionInGroup
  if(names(modeledData[[analysis]]$models$bestModel)=="interactionInPredAndGroup"){
    includeInteractionInGroup <- "(including interactions)"
  }

  c("analysis" = analysis,
    modelStructure$models$analyses[[analysis]]$narrative, # gather narratives
    "outcomeVariable" = modelStructure$variableExplanations[[outcome]],
    "predictorVariables" = predictorVariables,
    "includeInteractionInGroup" = includeInteractionInGroup,
    "groupingVariable" = modelStructure$variableExplanations[[grouping1]],
    "plotOutcome" = outcome, # for plotting
    "plotPredictor1" = predictor1, # for plotting, not used now
    "plotPredictor2" = predictor2 # for plotting, not used now
    )
}

# split the text, then join
cleanText <- Vectorize(function(text, reps){
  # split on the special sequence of characters.
  text <- strsplit(text, split="<>")[[1]]
  paste0(replaceText(text, reps = reps), collapse = "")
}, vectorize.args = "text", USE.NAMES = FALSE)

# replace the text with text from replacements
replaceText <- Vectorize(function(text, reps){
  if(!grepl("^\\$.*", text)){
    # if the variable character is not there, return the text.
    return(text)
  }
  replText <- reps[[substring(text, 2)]]

  return(replText)
}, vectorize.args = "text", USE.NAMES = FALSE)


writeMarkdown <- function(modeledData,
                          markdownPath = "./report.Rmd"
){

  analysisSkel <- readLines(system.file("markdown", "analysisSkeleton.Rmd", package = "mocapGrip", mustWork = TRUE))
  header <- readLines(system.file("markdown", "header.Rmd", package = "mocapGrip", mustWork = TRUE))

  content <- sapply(names(modeledData),
                    function(analysis) {
                      mocapGrip:::cleanText(analysisSkel, mocapGrip:::formatGatherReplacements(analysis, modeledData))},
                    simplify = TRUE, USE.NAMES = TRUE)
  markdownOut <- c(header, content)

  con <- file(markdownPath)
  on.exit(close(con))
  writeLines(markdownOut, con)
}




