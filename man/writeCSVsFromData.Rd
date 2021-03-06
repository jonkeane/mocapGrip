% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataTools.R
\name{writeCSVsFromData}
\alias{writeCSVsFromData}
\title{write csvs of data from a data object}
\usage{
writeCSVsFromData(data, namePrefix = "", path = "./", overwrite = FALSE)
}
\arguments{
\item{data}{a data object to use for writing data.}

\item{namePrefix}{a character string to prepend to each file. Default: ""}

\item{path}{a character string of the path to save the data. Default: "./"}

\item{overwrite}{logical, should the writer overwrite files that already exist? Default: `FALSE`}
}
\value{
None
}
\description{
write csvs of data from a data object
}

