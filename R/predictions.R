unscale <- function(scaledThing, input = NA){
  cntr <- attributes(scaledThing)[["scaled:center"]]
  sd <- attributes(scaledThing)[["scaled:scale"]]

  unscaled <- ifelse(is.na(input), scaledThing*sd+cntr, input*sd+cntr)

  as.vector(unscaled)
}


########## model visualization prep function for mixed models form lmer ##########
pred <- function(fitModel) {

  # create a data frame with various levels represented
  vars <- stringr::str_split(as.character(fitModel@call$formula), stringr::fixed(" "))
  outcomeVarName <- vars[[2]]
  allPredictors <- attributes(terms(fitModel@call$formula))$term.labels
  isInteraction <- stringr::str_detect(attributes(terms(fitModel@call$formula))$term.labels, ":")
  isLevel <- stringr::str_detect(attributes(terms(fitModel@call$formula))$term.labels, "\\|")
  predictorVars <- subset(allPredictors, !isInteraction & !isLevel)
  levelVars <- subset(allPredictors, isLevel)
  levelVars <- stringr::str_replace(levelVars, ".* \\| ", "") # remove all slope levels


  newdat <- fitModel@frame
  newdat[,outcomeVarName] <- NA
  newdat <- unique(newdat)

  # generate model values
  mm<-model.matrix(terms(fitModel),newdat)

  # generate predictions
  newdat[,outcomeVarName] <- (mm %*% lme4::fixef(fitModel))

  # generate variance from the fixed effects (add in random effects?)
  newdat$pvar1 <- diag(mm %*% tcrossprod(vcov(fitModel),mm))

  # add the random effects into the estimate
  for(var in levelVars){
#     print(ranef(fitModel)[[var]][as.character(newdat[,var]),'(Intercept)'])
    newdat[,outcomeVarName] <- newdat[,outcomeVarName]+lme4::ranef(fitModel)[[var]][as.character(newdat[,var]),'(Intercept)']

  }

  # make high and low estimates
  newdat <- data.frame(
    newdat
    , plo = newdat[,outcomeVarName]-2*sqrt(newdat$pvar1)
    , phi = newdat[,outcomeVarName]+2*sqrt(newdat$pvar1)
  )

  # collapse groups that are irrelevant using ddply(...,summarise,...) (ver_letter removed!)

  eval(parse(text = paste0("data.modeled <- ddply(newdat, predictorVars, summarise, ", outcomeVarName, " = mean(", outcomeVarName,"), plo=mean(plo), phi=mean(phi))")))
}

########## model visualization prep function for simple form lm ##########
predSimple <- function(fitModel) {

  # create a data frame with various levels represented
  vars <- stringr::str_split(as.character(fitModel$call$formula), stringr::fixed(" "))
  outcomeVarName <- vars[[2]]
  allPredictors <- attributes(terms(fitModel))$term.labels
  isInteraction <- stringr::str_detect(attributes(terms(fitModel))$term.labels, ":")
  isLevel <- stringr::str_detect(attributes(terms(fitModel))$term.labels, "\\|")
  predictorVars <- subset(allPredictors, !isInteraction & !isLevel)
  levelVars <- subset(allPredictors, isLevel)
  levelVars <- stringr::str_replace(levelVars, ".* \\| ", "") # remove all slope levels


  newdat <- fitModel$model

  newdat[,outcomeVarName] <- NA
  newdat <- unique(newdat)

  # generate model values
  mm<-model.matrix(terms(fitModel),newdat)

  # generate predictions
  newdat[,outcomeVarName] <- (mm %*% coefficients(fitModel))

  # generate variance from the fixed effects (add in random effects?)
  newdat$pvar1 <- diag(mm %*% tcrossprod(vcov(fitModel),mm))

  # make high and low estimates
  newdat <- data.frame(
    newdat
    , plo = newdat[,outcomeVarName]-2*sqrt(newdat$pvar1)
    , phi = newdat[,outcomeVarName]+2*sqrt(newdat$pvar1)
  )

  # collapse groups that are irrelevant using ddply(...,summarise,...) (ver_letter removed!)

  eval(parse(text = paste0("data.modeled <- ddply(newdat, predictorVars, summarise, ", outcomeVarName, " = mean(", outcomeVarName,"), plo=mean(plo), phi=mean(phi))")))
}
