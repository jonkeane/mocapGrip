#' Make a full report from data
#'
#' takes a data object, fits (and finds the best) model, and then produces a report based on the provided analysisSkeleton and narrative found in modelMetadata.
#'
#' @param data A list conforming to the structure of data for the project from \code{\link{extractMocapDataFromAnnotations}} (an example of this is the \code{\link{pureReplication}} object)
#' @param reportPath a string with the path and name of the report to be made (this should end in \code{.Rmd}), by default it is: \code{"./report.Rmd"}
#' @param title a string for the title of the report, the default is "Grip Project Report"
#' @param ... options to pass to \code{\link{modelAllData}}, (e.g. \code{last=TRUE} if the last model that converged should be selected rather than the default (\code{last=FALSE}) first).
#'
#' @return An object that is a code of \code{data}, but with the models appened to each analysis present. This is useful if you need to inspect the models or use this for further analysis or reporting.
#'
#' @export
# add kinds of models?
# add in different analysis skeletons?
makeReport <- function(data, reportPath="./report.Rmd", title = "Grip Project Report", ...){
  # parse data? this should be an option
  # data <- readExtractedMocapData("~/Dropbox/mocap/gripStudy/analysis/extractedData/", types)

  # check wellformedness of the data object.

  # fit models from analyze
  # pick the best model (add warnings here?)
  dataModeled <- modelAllData(data, ...)

  # read/write template
  writeMarkdown(dataModeled, markdownPath = reportPath)

  # render
  reportOut <- rmarkdown::render(reportPath, params = list(data = dataModeled, title = title))
  # change path

  message("Success, the report was written to ", reportOut, sep="")
  return(dataModeled)
}

#' Write markdown based on modeled data
#'
#' @param modeledData a list of modeled data of the format from \code{\link{modelAllData}}
#' @param markdownPath a string of the path to store the report markdown file. By default is is \code{"./report.Rmd"}
#' @param analysisSkel an R Markdown (\code{.Rmd}) file that has the skeleton for the analysis. This skeleton is the output for a single analysis. It will be repeated if multiple analysis types are in \code{modeledData}. It includes special variables which will be replaced with information from \code{modelMetadata}. These special variables look like \code{<>$intro<>}, which would be replaced with the text marked \code{intro} in \code{modelMetadata} for the specific analysis in modeledData.
#' @param header an R Markdown file that has header information so that the report can be generated.
# @param modelMetadata ??
#'
#' @return None
#' @export
writeMarkdown <- function(modeledData,
                          markdownPath = "./report.Rmd",
                          analysisSkel = readLines(system.file("markdown", "analysisSkeleton.Rmd", package = "mocapGrip", mustWork = TRUE)),
                          header = readLines(system.file("markdown", "header.Rmd", package = "mocapGrip", mustWork = TRUE))
                          ){
  # grab names, and exclude fullData
  dataSets <- names(modeledData)
  dataSets <- dataSets[dataSets!="fullData"]

  # this will break if there is more than one analysis specifically: `names(modeledData[[dataSet]]$analyses)`
  content <- sapply(dataSets,
                    function(dataSet) {
                      sapply(names(modeledData[[dataSet]]$analyses),
                             function(analysis){
                               cleanText(analysisSkel, formatGatherReplacements(dataSet, analysis, modeledData))
                               },
                             simplify = TRUE, USE.NAMES = TRUE)
                      },
                    simplify = TRUE, USE.NAMES = TRUE)

  markdownOut <- c(header, unlist(content))

  con <- file(markdownPath)
  on.exit(close(con))
  writeLines(markdownOut, con)
}


# takes an analysis specification and (all of the) modeled data and then returns a (named) list of replacements to be used by replaceText()
formatGatherReplacements = function(dataSet, analysis, modeledData) {
  # grab variables from the analyses structure
  outcome <- modelMetadata$models$analyses[[analysis]]$variablesToUse$outcome
  predictor1 <- modelMetadata$models$analyses[[analysis]]$variablesToUse$predictor1
  predictor2 <- modelMetadata$models$analyses[[analysis]]$variablesToUse$predictor2
  grouping1 <- modelMetadata$models$analyses[[analysis]]$variablesToUse$grouping1
  # grab the formula from the best model.
  formula <- stats::formula(modeledData[[dataSet]]$analyses[[analysis]]$bestModel[[1]]$modelObject)

  # check if there is an interaction in the predictors.
  if(names(modeledData[[dataSet]]$analyses[[analysis]]$bestModel) %in% c("interactionInPredAndGroup", "interactionInPred")){
    interaction <- modelMetadata$variableExplanations[[paste(predictor1, predictor2, sep = "X")]]
  } else {
    interaction <-  NULL # NULL so that no extra bullet is added.
  }

  # make and format $predictorVariables
  predictorVariables <- paste0("\n* ",
                               c(modelMetadata$variableExplanations[[predictor1]],
                                 modelMetadata$variableExplanations[[predictor2]],
                                 interaction),
                               collapse = "")


  # add interaction text if  the selected model is $includeInteractionInGroup
  if(names(modeledData[[dataSet]]$analyses[[analysis]]$bestModel)=="interactionInPredAndGroup"){
    includeInteractionInGroup <- "(including interactions)"
  } else {
    includeInteractionInGroup <- "" # empty string so that nothing is printed.
  }

  # adjust predictor one (which is usually sticks) if it is scaled to the unscaled version.
  plotPredictor1 <- ifelse(predictor1 == "stickcmScaled", "stickcmScaled+8", predictor1)

  c("dataSet" = dataSet,
    "analysis" = analysis,
    modelMetadata$dataSets[[dataSet]]$narrative, # gather narratives
    "outcomeVariable" = modelMetadata$variableExplanations[[outcome]],
    "predictorVariables" = predictorVariables,
    "includeInteractionInGroup" = includeInteractionInGroup,
    "groupingVariable" = modelMetadata$variableExplanations[[grouping1]],
    "plotOutcome" = outcome, # for plotting
    "plotPredictor1" = plotPredictor1, # for plotting, not used now
    "plotPredictor2" = predictor2, # for plotting, not used now
    "formula" = deparse(formula, width.cutoff = 500) # for plotting, not used now
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
