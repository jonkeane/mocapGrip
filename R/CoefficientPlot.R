#' @import ggplot2

countMatches <- Vectorize(function(pat, vec) sapply(regmatches(vec, gregexpr(pat, vec)), length))

CoefficientPlot <- function(models, modelnames = "", variables = NA, interactionCutoff = Inf, removeintercept = FALSE){
  # models must be a list()

  Alphas <- c(1,5) / 100
  Multiplier <- qnorm(1 - Alphas / 2)

  # This should be adjusted to accept both S3 and S4 classes.
  CoefficientTables <- lapply(models, function(x){summary(x)$coef})
  #  CoefficientTables <- lapply(models, function(x){summary(x)@coefs})

  # Check if CoefficientTables is a list or not to allow for multiple plots when there are multiple coefficient tables (eg beta regressions).
  # currently this only works with non-heterogenous lists. That is, models that have multiple coefficient talbes cannot be used with models that don't.
  # A better way might be making comparison plots for each list within the list. This would require refactoring the plotting function to be split and applied to each sublist separately.
  if(all(unlist(lapply(CoefficientTables, is.list)))){
    before <- length(CoefficientTables)
    CoefficientTables <- unlist(CoefficientTables, recursive = FALSE)
    after <- length(CoefficientTables)
    # figure out how many tables per model there are. This is fragile, and assumes the same number of tables per each model.
    tablesPerModel <- after/before
  } else {
    tablesPerModel <- 1
  }

  TableRows <- unlist(lapply(CoefficientTables, nrow))

  if(modelnames[1] == ""){
    nums <- 1:(length(TableRows)/tablesPerModel)
    # generate the model names by pasting the coefficientTable names (if they exist), and then duplicate for the number of rows in each table.
    ModelNameLabels <- rep(paste(paste("Model", nums), names(CoefficientTables)), TableRows)
  } else {
    if(length(models) != length(modelnames)) { stop("The number of models doesn't match the number of model labels")}

    # generate the model names by pasting the coefficientTable names (if they exist), and then duplicate for the number of rows in each table.
    ModelNameLabels <- rep(paste(modelnames, names(CoefficientTables)), TableRows)
  }

  numModels <- length(unique(ModelNameLabels))

  #   dodgeWidth <-  ifelse(numModels==1, 0, ifelse(numModels==2, 0.5, ifelse(numModels==3, 0.6, ifelse(numModels==4, 0.7, ifelse(numModels==5, 0.8, ifelse(numModels==6, 0.9, ifelse(numModels==7, 1, NA)))))))
  # This works until the number of models gets unreasonably large.
  dodgeWidth <-  ifelse(numModels==1, 0, ifelse(numModels>7, 0.9, 0.5+(numModels-2)*0.1))
  expandRatio <-  ifelse(numModels<7, 1, 1+(numModels-6)*0.1)
  # coord_fixed(ratio = 1)



  MatrixofModels <- cbind(do.call(rbind, CoefficientTables), ModelNameLabels)
  if(removeintercept == TRUE){
    MatrixofModels <- MatrixofModels[!rownames(MatrixofModels) == "(Intercept)", ]
  }

  # get predictor names from rownames, but remove them before cbinding to avoid warnings about duplicates
  Predictors <- rownames(MatrixofModels)
  row.names(MatrixofModels) <- NULL
  MatrixofModels <- data.frame(cbind(Predictors, MatrixofModels))

  MatrixofModels <- data.frame(cbind(MatrixofModels, rep(Multiplier, each = nrow(MatrixofModels)), row.names = NULL))

  MatrixofModels <- MatrixofModels[c("Predictors", "Estimate", "Std..Error", "ModelNameLabels", "rep.Multiplier..each...nrow.MatrixofModels..")]

  colnames(MatrixofModels) <- c("Predictors", "Estimate", "StandardError", "ModelName", "Scalar")
  MatrixofModels$Predictors <- factor(MatrixofModels$Predictors, levels = unique(MatrixofModels$Predictors))

  if(!any(is.na(variables))){
    MatrixofModels <- subset(MatrixofModels, Predictors %in% variables)
  }

  MatrixofModels <- subset(MatrixofModels, countMatches(":", Predictors) < interactionCutoff)

  MatrixofModels[, -c(1, 4)] <- apply(MatrixofModels[, -c(1, 4)], 2, function(x){as.numeric(as.character(x))})

  OutputPlot <- ggplot(MatrixofModels, aes(ymax = max(Estimate + Scalar * StandardError))) # ymax to disable warning message
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = "black", alpha = I(3/12))

  if(numModels>1){
    OutputPlot <- OutputPlot + geom_linerange(aes(x = Predictors, y = Estimate, ymin = Estimate - Scalar * StandardError, ymax = Estimate + Scalar * StandardError, group = ModelName, color=ModelName, size=-(Scalar)), position = position_dodge(width = dodgeWidth))
    OutputPlot <- OutputPlot + geom_point(aes(x = Predictors, y = Estimate, group = ModelName, color=ModelName), position = position_dodge(width = dodgeWidth))
    OutputPlot <- OutputPlot + scale_colour_discrete(name = "Model")
  } else {
    {
      OutputPlot <- OutputPlot + geom_linerange(aes(x = Predictors, y = Estimate, ymin = Estimate - Scalar * StandardError, ymax = Estimate + Scalar * StandardError, size=-(Scalar)), position = position_dodge(width = dodgeWidth))
      OutputPlot <- OutputPlot + geom_point(aes(x = Predictors, y = Estimate), position = position_dodge(width = dodgeWidth))
    }
  }

  OutputPlot <- OutputPlot + scale_size_continuous(range = c(0.5, 1), guide = FALSE) + ylab(NULL) + xlab(NULL)
  OutputPlot <- OutputPlot  + coord_flip()

  return(OutputPlot)
}
