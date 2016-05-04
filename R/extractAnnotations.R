
#' Extract (and check) annotations from elan files.
#'
#' This function takes in a vector of elan files (*.eaf) and extracts the annotations from them. Additionally it preforms some checking to make sure that the annotations are in the format we expect. This won't catch all possible annotation errors, but should catch many of the most common ones.
#'
#' @param files A vector of elan files
#' @param destDir The destination directory where the extract csvs should be saved
#' @return Nothing, not useful currently.
#'
#' The \code{files} option can be a single (fully specified) elan file, a vector of fully specific elan files, or it can be a path with wildcards (e.g. \code{./GRI_0??/GRI_0??-SESSION_0??-TRIAL_0??.eaf}) that matches at least one, but potentially many elan files
#'
#' @export
extractAnnotations <- function(files, destDir){
  pathToExtractScript <- system.file("python","extractAnnotations.py", package = "mocapGrip", mustWork=TRUE)

  arugments <- c(pathToExtractScript, destDir, Sys.glob(files))
  system2("python", args = arugments)
  call

  return()
}




