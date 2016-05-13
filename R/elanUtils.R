#' Fix the paths of elan files
#'
#' This function takes in a vector of elan files and it searches the path where the files are for video, audio, csv, and tsconf files to link to if the links do not currently work.
#'
#' It uses a search that matches the filename and as much as the path as it can, but the search is limited to the folder structure for the project. In other words, it will only search in the folders AUDIO, Clipped Videos, mocapCSVs, (which must be located in the parent of the parent folder where the elan files are located) additionally, it searches in the folder containing the elan file for tsconf file links.
#'
#' @param files A vector of video files
#' @return Nothing, not useful currently.
#'
#' @export
fixPaths <- function(files){
  pathToExtractScript <- system.file("python","relPathFixGRIP.py", package = "mocapGrip", mustWork=TRUE)

  arugments <- c(pathToExtractScript, Sys.glob(files))
  system2("python", args = arugments)
  call

  print("Warning: a backup of each eaf and tsconf file has been created (with the same name as the original plus .bak at the end). If this command is run again, those backup files will be overwritten.")
  return()
}



#' Install pyelan from source
#'
#' By default, installing this r package directly from github will not install the submodule dependency of pyelan.
#'
#' @return Nothing, not useful currently.
#'
#' @export
installPyelan <- function(){
  pyelanFolder <- file.path(system.file("python", package = "mocapGrip"), "pyelan")
  # check if folder exists, and create it if it doesn't
  dir.create(pyelanFolder, showWarnings = FALSE)
  git2r::clone("https://github.com/jonkeane/pyelan.git", pyelanFolder)
}
