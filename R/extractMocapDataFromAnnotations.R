
#' Extract (and check) annotations from elan files.
#'
#' Takes in a vector of elan files (*.eaf) and extracts the annotations from them. Additionally it preforms some checking to make sure that the annotations are in the format we expect. This won't catch all possible annotation errors, but should catch many of the most common ones.
#'
#' @param files A vector of elan files
#' @param destDir The destination directory where the extract csvs should be saved
#' @return Nothing, not useful currently.
#'
#' The \code{files} option can be a single (fully specified) elan file, a vector of fully specific elan files, or it can be a path with wildcards (e.g. \code{./GRI_0??/GRI_0??-SESSION_0??-TRIAL_0??.eaf}) that matches at least one, but potentially many elan files
#'
#' @export
extractMocapDataFromAnnotations <- function(files, destDir){
  pathToExtractScript <- system.file("python","extractMocapDataFromAnnotations.py", package = "mocapGrip", mustWork=TRUE)

  # test if files is folder
  if(file.info(files)$isdir) {stop("The file that was passed (", files ,") appears to be a folder. This should be a single elan file, a vector (list) of elan files, or a path with wild cards that lead to multiple elan files.")}

  lapply(files, function(file) {
    if(file) {stop("The file that was passed (", file ,") does not end in .eaf, this function only accepts eaf files.")}
  })

  arugments <- c(pathToExtractScript, destDir, Sys.glob(files))
  system2("python", args = arugments)
  call

  message("extractMocapDataFromAnnotations() has completed. Check the output above for any warnings.")
}




