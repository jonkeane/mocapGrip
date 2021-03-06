% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze.R
\name{fitModels}
\alias{fitModels}
\title{Fit the right models based on the name of the dataset being passed}
\usage{
fitModels(dataSet, analysis, data, modelMd)
}
\arguments{
\item{dataSet}{this is the data set to use. Exampls: "action", "estimation"}

\item{analysis}{this is the analysis of data that is going to be analyzed from \code{data}. Examples: "maxGrip.stickAsContinuous", "meanGrip.stickAsContinuous" The name of this option determines which model will be fit.}

\item{data}{a data object as generated by \code{\link{readExtractedMocapData}}}

\item{modelMd}{a modelMetadata object, by default it uses the \code{modelMetadata} that comes with the package.}
}
\value{
a list of fit models
}
\description{
Fit the right models based on the name of the dataset being passed
}

