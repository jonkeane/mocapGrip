
#' Extract (and check) annotations from elan files.
#'
#' This function takes in a vector of elan files and extracts the annotations from them. Additionally it preforms some checking to makes ure that the annotations are in the format we expect. This won't catch all possible annotation errors, but should catch many of the most common ones.
#'
#' @param files A vector of video files
#' @param destDir The destination directory where the extract csvs should be saved
#' @return Nothing, not useful currently.
#'
#' @export
extractAnnotations <- function(files, destDir){
  pathToExtractScript <- system.file("python","extractAnnotations.py", package = "mocapGrip", mustWork=TRUE)

  arugments <- c(pathToExtractScript, destDir, Sys.glob(files))
  system2("python", args = arugments)
  call

  return()
}




