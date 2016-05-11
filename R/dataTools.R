#' write csvs of data from a data object
#'
writeCSVsFromData <- function(data, namePrefix = "", path="./", overwrite = FALSE) {
  dataNames <- names(data)
  filenamesout <- paste0(path, namePrefix, dataNames, ".csv")

  # test if file exists
  if(!overwrite & any(file.exists(filenamesout))) {
    stop("At least one of the files (", filenamesout, ") already exist. Use overwrite = TRUE to overwrite them.")
  }

  result <- mapply(function(dataSet, filenameout) {readr::write_csv(dataSet$data, filenameout)}, data, filenamesout)

  sapply(names(result), function(dataSet){
    message("Successfully wrote the ", dataSet, " data to ", result[[dataSet]])
  }, simplify = TRUE, USE.NAMES = FALSE)

  return(invisible())
}

# writeCSVsFromData(pureReplication, namePrefix = "test", overwrite=TRUE)
