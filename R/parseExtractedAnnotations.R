process <- function(file, conditionCodesFile, verbose = FALSE) {
  # read in csvs as strings including NA (which indicates occlusion) as a string (and empty as a true NA)
  # readr::read_csv doesn't work as a simple replacement.
  data <-
    utils::read.csv(file = file,
                    na.strings = "",
                    stringsAsFactors = FALSE)
  data$times <-
    seq(
      from = 0,
      to = (1 / 120) * (nrow(data) - 1),
      length.out = nrow(data)
    )
  # melt ignoring true NAs
  meltedData <-
    reshape2::melt(data,
                   id = c("times"),
                   na.rm = TRUE,
                   value.name = "grip")

  #Turn string NAs (occlusions) into real NAs, and then numericize
  meltedData$grip <-
    ifelse(meltedData$grip == "NA", NA, meltedData$grip)
  meltedData$grip <- as.numeric(meltedData$grip)

  meltedData$variable <- as.character(meltedData$variable)
  meltedData$variable <-
    as.factor(ifelse(
      grepl(".*\\..$", meltedData$variable),
      meltedData$variable,
      paste(meltedData$variable, "0", sep = ".")
    ))

  meltedData$variable <- gsub("X", "", meltedData$variable)
  meltedData$variable <- gsub("(\\.)+", ".", meltedData$variable)

  if (verbose) {
    print(file)
  }
  if (verbose) {
    print(unique(meltedData$variable))
  }
  #   meltedData <- meltedData %>% separate(variable, into=c("condition","type","side","period","num"), sep="\\.", extra = "drop")

  meltedData <-
    meltedData %>% tidyr::extract_(
      "variable",
      into = c("condition", "type", "period", "gripType", "num"),
      regex = "([[:digit:]][[:digit:]]?)\\.(ACTION|GESTURE|ESTIMATION)\\.(EYESCLOSED|OBSERVE|GRIP|MOVEMENT|RELEASE|PLANNING|PREPARE|STEADY|TRANSITION|UNCODABLE|NO.GESTURE|TRANSITION.GRIP)\\.?(CLOSED|OPEN|OPEN.CLOSED|CLOSED.OPEN)?\\.([[:digit:]])",
      convert = TRUE
    )

  meltedData$condition <- as.factor(meltedData$condition)
  meltedData$type <- as.factor(meltedData$type)
  meltedData$period <- as.factor(meltedData$period)

  meltedData$file <- file

  meltedData <-
    meltedData %>% tidyr::separate_(
      "file",
      into = c("obsisSubj", "obsisSession", "obsisTrial"),
      sep = "-",
      remove = FALSE
    )

  meltedData$obsisSubj <-
    sub(".*GRI_([[:digit:]][[:digit:]][[:digit:]])",
        "\\1",
        meltedData$obsisSubj)

  meltedData$obsisSession <-
    gsub("SESSION_", "", meltedData$obsisSession)
  meltedData$obsisTrial <- gsub("TRIAL_", "", meltedData$obsisTrial)
  meltedData$obsisTrial <- gsub(".csv", "", meltedData$obsisTrial)
  meltedData$obsisTrial <- gsub("TEST", "", meltedData$obsisTrial)

  meltedData$file <- as.factor(meltedData$file)

  condCodes <- utils::read.csv(conditionCodesFile)

  # change to dplyr inner_join?
  meltedData <- plyr::join(meltedData, condCodes, by = "condition")

  meltedData
}

# grab only grip and time data
importCols <- function(data) {
  # return data without the times or grip column
  data[,!(names(data) %in% c("times", "grip"))]
}

#' takes a dataSet and data, and gives back a list with the extracted data in named lists.
#'
#' @param dataSet character the name of the dataSet
#' @param data a data object for the data to be retrieved from
#' @param modelMd a modelMetadata object, default: the modelMetadata from the package
#'
#' @return the extracted data from the dataSet
processDataSet <- function(dataSet, data, modelMd = modelMetadata) {
  # make a list to store dataSet data in
  dataSetData <- list()

  # make a list for warnings to be stored
  warns <- list()

  # grab the dataSet specificationsi from the modelMd object
  filterString <-
    modelMd$dataSets[[dataSet]]$processing$filterString
  func <- modelMd$dataSets[[dataSet]]$processing$processFunction

  # grab the process objects, and make them into a string to pass to do.
  processFunctionOptions <-
    modelMd$dataSets[[dataSet]]$processing$processFunctionOptions
  processFuncOptString <- paste0(sapply(names(processFunctionOptions),
                                        function(argName) {
                                          nm <- argName
                                          op <- processFunctionOptions[[argName]]
                                          if(is.numeric(op)){
                                            paste0(nm, " = ", op)
                                          } else {
                                            paste0(nm, " = '", op, "'")
                                          }
                                          }, simplify = TRUE, USE.NAMES = FALSE), collapse = ", ")


  # Try and find a the given processing file
  # the error could be more specific
  # asNamespace might not be needed.
  tryCatch({
    get(func, envir = asNamespace('mocapGrip'), mode = 'function')
  },
  error = function(e) {
    stop(
      "Could not find the function ",
      func,
      ", which was specified to process the data for the dataSet ",
      dataSet,
      sep = ""
    )
  })

  # process the data with the function that was found, add it to dataSetData (along with warnings)
  dataSetData[["data"]] <- withCallingHandlers({
    data %>%
      dplyr::filter_(stats::as.formula(paste0("~", filterString))) %>%
      dplyr::group_by_(.dots = list("obsisSubj", "obsisTrial", "condition")) %>%
      dplyr::do_(
        stats::as.formula(paste0("~", func, "(., ", processFuncOptString,")"))
      )
  },
  warning = function(w) {
    warns <<- append(warns, w$message)
    invokeRestart("muffleWarning")
  })
  dataSetData[["warnings"]] <- warns

  # grab default analyses
  dataSetData[["analysesToRun"]] <-
    modelMd$dataSets[[dataSet]]$defaultAnalysis

  return(dataSetData)
}



#' Read extracted motion capture data
#'
#' Reads in extracted motion capture data from a directory.
#'
#' @param path Directory containing motion catpure csv files that were extracted with the \code{\link{extractMocapDataFromAnnotations}} function.
#' @param dataSets A vector of the types of periods (aka: dataSets) to extract for analysis. Default: c("action", "estimation") Possible values are: "action", "estimation", "release", "estMaxGrip", "gestMaxGrip", and "gestMove"
#' @param includeFullData A logical, should the output include the full data? default:\code{FALSE}
#' @param modelMd a modelMetadata object, default: the modelMetadata from the package

#' @return a data object. Wellformedness of this object can be checked with \code{\link{checkData}}
#'
#' @export
readExtractedMocapData <-
  function(path,
           dataSets = c("action", "estimation"),
           includeFullData = FALSE,
           modelMd = modelMetadata) {
    # to be added to main function for oparsing data
    files <-
      list.files(path,
                 recursive = TRUE,
                 pattern = NULL,
                 full.names = TRUE)

    data <-
      plyr::ldply(
        files,
        process,
        conditionCodesFile = system.file(
          "GRIPMLstimuli.csv",
          package = "mocapGrip",
          mustWork = TRUE
        ),
        verbose = FALSE,
        .progress = "text"
      )

    # run the preprocessing commans
    for (line in modelMd$dataPreProcessing) {
      eval(parse(text = line))
    }

    # add check if there are no known types found.
    dataSetData <-
      sapply(
        dataSets,
        processDataSet,
        data = data,
        modelMd = modelMd,
        USE.NAMES = TRUE,
        simplify = FALSE
      )

    if (includeFullData == TRUE) {
      dataSetData[["fullData"]] <- data
    }

    return(dataSetData)
  }



#' Extract new dataSets from a data object that includes fullData
#'
#' Add new dataSets to a data object that includes fullData.
#'
#' @param data a data object that has a fullData object.
#' @param dataSets A vector of the types of periods (aka: dataSets) to extract for analysis. Possible values are: "action", "estimation", "release", "estMaxGrip", "gestMaxGrip", and "gestMove"
#' @param modelMd a modelMetadata object, default: the modelMetadata from the package

#' @return a data object. Wellformedness of this object can be checked with \code{\link{checkData}}
#'
#' @export
addNewDataSets <- function(data, dataSets, modelMd = modelMetadata) {
  # test if data has fullData
  if (!{
    "fullData" %in% names(data)
  }) {
    stop(
      "The data object, ",
      deparse(substitute(data)),
      " does not have the fullData attached. Please use readExtractedMocapData(..., includeFullData = TRUE) to read the mocap data and save the fullData in the object."
    )
  }

  # find the dataSets already included
  dataSetsAlready <- names(data)[names(data) != "fullData"]
  # combine the includes dataSets (first) and the additional dataSets, ignoring duplicates
  dataSetsBoth <- unique(c(dataSetsAlready, dataSets))

  dataSetData <- sapply(dataSetsBoth, function(dataSet) {
    if (dataSet %in% dataSetsAlready) {
      # if the dataSet is already in the data object
      if (dataSet %in% dataSets) {
        # if the dataSet is already in the data object *and* is specified to extract warn that it's not being re-extracted
        warning(
          "The dataSet ",
          dataSet,
          " is already in the data object ",
          deparse(substitute(data)),
          ". It will not be over written. If you do want to reprocess this dataSet, please delete this dataSet from the data object, and rerun this addNewDataSets()."
        )
      }
      # if the dataSet is already in the data object return the dataSet as is
      return(data[[dataSet]])
    } else {
      # extract new dataSet
      processedData <- processDataSet(dataSet, data = data[["fullData"]], modelMd = modelMd)
      return(processedData)
    }
  }, USE.NAMES = TRUE, simplify = FALSE)

  dataSetData[["fullData"]] <- data$fullData

  return(dataSetData)
}
