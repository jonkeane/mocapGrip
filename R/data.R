#' Pure replication data for reach to grasp and estimation of sticks inside of a Mueller-Lyer illusion
#'
#' A dataset that contains 11 subjects reaching to grasp or estimating the size of 5, 7, 9, and 11 cm sticks. The sticks are sometimes placed in a Mueller-Lyer illusion.
#'
#' @format A list of data frames. There are two lists \emph{action} and \emph{estimation}
#'
#' @section Action:
#'
#'  Maximum grip aperture data from the reach to grasp period of the \emph{action} trials
#'
#'    \describe{
#'      \item{duration}{duration of the grip period}
#'      \item{maxGrip}{maximum grip aperture (in mm)}
#'      \item{maxGripTime}{time when maximum grip aperture occurred (in msec)}
#'      \item{maxGripTimeRel}{proportion of the grip period when the maximum grip happened}
#'      \item{condition}{condition code}
#'      \item{type}{trial type (all are \emph{ACTION})}
#'      \item{period}{the annotated period (all are \emph{GRIP})}
#'      \item{gripType}{NA (place holder for gesture trials)}
#'      \item{num}{unused}
#'      \item{file}{file this observation was extracted from.}
#'      \item{obsisSubj}{subject code}
#'      \item{obsisSession}{session (assigned by the motion capture software)}
#'      \item{obsisTrial}{trial number (assigned by the motion capture software)}
#'      \item{fins}{the configuration of the Mueller-Lyer illusion fins}
#'      \item{stick}{the stick (size) being presented, in a string, as a factor}
#'      \item{stickcm}{the size of the stick in centimeters}
#'      \item{orientation}{orientation of the presentation (all are \emph{horizontal})}
#'      \item{stickcmScaled}{stick size centered at 8cm (the middle of our stick sizes), in cm}
#'    }
#'
#' @section Estimation:
#'
#' Grip aperture data from the steady portion of the \emph{estimation} trials
#'
#'    \describe{
#'      \item{duration}{duration of the grip period}
#'      \item{meanGrip}{mean grip aperture (in mm)}
#'      \item{medianGrip}{median grip aperture (in mm)}
#'      \item{condition}{condition code}
#'      \item{type}{trial type (all are \emph{ESTIMATION})}
#'      \item{period}{the annotated period (all are \emph{STEADY})}
#'      \item{gripType}{NA (place holder for gesture trials)}
#'      \item{num}{unused}
#'      \item{file}{file this observation was extracted from.}
#'      \item{obsisSubj}{subject code}
#'      \item{obsisSession}{session (assigned by the motion capture software)}
#'      \item{obsisTrial}{trial number (assigned by the motion capture software)}
#'      \item{fins}{the configuration of the Mueller-Lyer illusion fins}
#'      \item{stick}{the stick (size) being presented, in a string, as a factor}
#'      \item{stickcm}{the size of the stick in centimeters}
#'      \item{orientation}{orientation of the presentation (all are \emph{horizontal})}
#'      \item{stickcmScaled}{stick size centered at 8cm (the middle of our stick sizes), in cm}
#'    }
"pureReplication"
