# data <- readExtractedMocapData(path="~/Dropbox/mocap/gripStudy/analysis/extractedData/")

# take a string of formulas to try, and iterate over them. (does lapply work sequentially?)

# Fit an lmer model with an equation and data, return list of the model, and convergence diagnostics (with the equation as the name)
fitLMERsingle <- function(eq, data){
  oldWarn <- getOption("warn") # save the old warning state
  options(warn = -1) # ignore all warnings

  fitOut <- lme4::lmer(stats::as.formula(eq), data)
  convLme4 <- fitOut@optinfo$conv$lme4
  if(is.list(convLme4) & length(convLme4) != 0){
    # check if the lme4 convergence list has any contents
    warnCode <- convLme4$code
    warnMessages <- convLme4$messages
  } else {
    # if the lme4 convergence list has no contents, setup success variables
    warnCode <- 0
    warnMessages <- ""
  }

  options(warn = oldWarn) # ignore all warnings
  return(list(modelObject=fitOut, converged={warnCode==0}, convWarnCode=warnCode, convWarnMsgs=warnMessages))
}

# Vectorized version of fitLMERsingle
fitLMER <- Vectorize(fitLMERsingle, vectorize.args = "eq", SIMPLIFY = FALSE)

# generate equations (formulas) with two predictors from maximal to simpler
eqsGen2preds <- function(outcome, predictor1, predictor2, grouping1 = "obsisSubj"){
  eqs <- character()
  eqs <- append(eqs, paste0(outcome, "~", predictor1, "*", predictor2, "+", "(", "1+", predictor1, "*", predictor2, "|", grouping1, ")"))
  eqs <- append(eqs, paste0(outcome, "~", predictor1, "*", predictor2, "+", "(", "1+", predictor1, "+", predictor2, "|", grouping1, ")"))
  eqs <- append(eqs, paste0(outcome, "~", predictor1, "+", predictor2, "+", "(", "1+", predictor1, "+", predictor2, "|", grouping1, ")"))
  return(eqs)
}


# fit the right models for the data.
fitTheRightModels <- function(type, data, additionalModelTypes=list()){

  # should these be stored as json for possible external editing?
  modelsByType <- list(
    "action" = list("outcome" = "maxGrip",
                      "predictor1" = "stickcmScaled",
                      "predictor2" = "fins"),
    "estimation" = list("outcome" = "meanGrip",
                    "predictor1" = "stickcmScaled",
                    "predictor2" = "fins")
  )

  # add additional models, a bit of a hack for expansion?
  if(length(additionalModelTypes)>0){
    modelsByType <- c(modelsByType, additionalModelTypes)
  }

  # check if the type of analysis is one of the known ones.
  # update this message if the models by type is moved elsewhere.
  if(!{type %in% names(modelsByType)}){
    stop("Error, the type ", type, " can't be found in the models by type specification for what variables to use. Please update the modelsByType to include this model type.", sep="")
  }

  modelsOut <- fitLMER(eqsGen2preds(outcome=modelsByType[[type]]$outcome, predictor1=modelsByType[[type]]$predictor1, predictor2=modelsByType[[type]]$predictor2), data=data[[type]]$data)

  return(modelsOut)
}
