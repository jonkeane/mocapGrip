
extractMarkers <- function(data, markers, verbose=FALSE){
  dataNew <- data
  print(nrow(dataNew))
  if(nrow(dataNew)<1){
    stop("Error, there are not enough rows of data in the (filtered) markers file. Please check that file and possibly reprocess this trial with obsis.")
  }
  dfOut <- data.frame(times = as.numeric(unlist(dataNew["Time..sec."]))-min(unlist(dataNew["Time..sec."])))
  for(marker in markers){
    if(verbose){
      print(data[,c("subj","session","trial")])
      print(marker)
    }
    markers <- dataNew[c(paste("X",marker[1],sep=""),paste("Y",marker[1],sep=""),paste("Z",marker[1],sep=""))]
    dfOut <- cbind(dfOut, markers)
  }
  dfOut
}

#' @importFrom magrittr %>%
calculateDistances <- function(data, markers){
  # make marker names and label name
  x1 <- paste("X", markers[1] ,sep="")
  y1 <- paste("Y", markers[1] ,sep="")
  z1 <- paste("Z", markers[1] ,sep="")
  x2 <- paste("X", markers[2] ,sep="")
  y2 <- paste("Y", markers[2] ,sep="")
  z2 <- paste("Z", markers[2] ,sep="")
  label <- paste(markers, collapse="-")

  # setup distance evaluation funciton
  distanceValue <- lazyeval::interp(~dist(rbind(c(x1, y1, z1), c(x2, y2, z2))),
                                    x1=as.name(x1), y1=as.name(y1), z1=as.name(z1),
                                    x2=as.name(x2), y2=as.name(y2), z2=as.name(z2)
                                    )

  dfOut <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate_(.dots = stats::setNames(list(distanceValue), label))

  dfOut
}

meanOnAxis <- function(data, markers, axis){
  # make marker names and label name
  markersWithAxis <- paste(axis, markers ,sep="")
  label <- paste("mean",axis,paste(markers, collapse="-"), sep="-")

  # Get means for each row of only the marker/axes that we care about
  data[,label] <- rowMeans(data[,markersWithAxis], na.rm = TRUE)

  data
}

# A gross alignment function that looks for and finds peaks that are over the threshold amplitude
alignGross <- function(distances, times, firstOpen=TRUE, openThreshold = 100){
  if(length(distances)!=length(times)){
    stop("Error, the distances and times are not of the same length.")
  }

  # check if there are any infinite states in the clapper state.
  if(any(is.infinite(distances))) {
    numInfss <- sum(is.infinite(distances))
    warning(paste("Warning, there are ",numInfss," infinities in the clapper state. If this number is sufficiently low, this might not be a problem.", sep = ""))
	distances <- ifelse(is.infinite(distances),NA,distances)
  }

  # check the number of transitions at the threshold, if over 2, error.
  clapperOpen <- ifelse(distances>openThreshold, 1, 0)
  if(any(is.na(clapperOpen))) {
    numNAs <- sum(is.na(clapperOpen))
    warning(paste("Warning, there are ",numNAs," NAs in the clapper state. If this number is sufficiently low, this might not be a problem.", sep = ""))
  }


  clapperOpen
}

# alignGross(filteredMarkers$clapperState , filteredMarkers$times)
# alignGross(filteredMarkers$clapperState , filteredMarkers$times, firstOpen=FALSE)

# a fine threshold finder that finds the frame that is the smallest of all the frames in a window before (direction="backword") or after (direction="forward") it.
minThresh <- function(distances, times, start, direction="backward", windowWidth=10, verbose=FALSE){
  if(length(distances)!=length(times)){
    stop("Error, the distances and times are not of the same length.")
  }
  if(direction=="backward"){
    distances <- rev(distances[times<=start])
    times <- rev(times[times<=start])
  } else if(direction=="forward"){
    distances <- distances[times>=start]
    times <- times[times>=start]
  }
  if(verbose){
    graphics::plot(distances, type="l")
  }

  allInc <- FALSE
  n <- 1
  while(!allInc & n < length(distances)){
    dir <- c()
    for(nn in c(1:windowWidth)){
      dir[nn] <- ifelse(distances[n]>distances[n+nn], "lower", "higher")
    }
    if(all(dir=="higher", na.rm=TRUE)){
      allInc <- TRUE
    } else {
      n <- n+1
    }
  }
  if(!allInc){
    stop("Error, no minimum found, try adjusting the size of the window unit.")
  }
  if(verbose){
    graphics::points(x=n, distances[n])
  }
  times[n]
}

# minThresh(filteredMarkers$clapperState , filteredMarkers$times, 10.39167, verbose=TRUE)
# minThresh(filteredMarkers$clapperState , filteredMarkers$times, direction="forward", 574.7167, verbose=TRUE)


# minThresh(filteredMarkers$clapperState , filteredMarkers$times, alignGross(filteredMarkers$clapperState , filteredMarkers$times), verbose=TRUE)
# minThresh(filteredMarkers$clapperState , filteredMarkers$times, direction="forward", alignGross(filteredMarkers$clapperState , filteredMarkers$times, firstOpen=FALSE), verbose=TRUE)

# master align funciton that one frame offset: 1001/60000
align <- function(data, windowWidth=10, verbose=TRUE, offset=0){
  times <- data$times
  distances <- data$clapperState


  clapperStates <- alignGross(distances , times)


  clapperOpenTrans <- table(paste0(head(clapperStates,-1),tail(clapperStates,-1)))
  if(clapperOpenTrans["01"]+clapperOpenTrans["10"]<4) {
    warning("Warning, there are less than two open states on the clapper. Using the only state as the beginning of the clip.")
	nClapperStates <- 1
  } else if(clapperOpenTrans["01"]+clapperOpenTrans["10"]>4) {
  	nClapperStates <- (clapperOpenTrans["01"]+clapperOpenTrans["10"])/2
    warning(paste("Warning, there are ",nClapperStates," open states on the clapper. Using the first and the last states for clipping. Try adjusting the threshold up or down.", sep=""))
  } else {
	nClapperStates <- 2
  }

  minTime <- minThresh(distances , times, windowWidth=windowWidth, min(times[clapperStates==1 & !is.na(clapperStates)], na.rm = FALSE), verbose=TRUE)
  if(nClapperStates == 1) {
	maxTime <- max(times)
  } else {
	maxTime <- minThresh(distances, times, windowWidth=windowWidth, max(times[clapperStates==1 & !is.na(clapperStates)], na.rm = FALSE), direction="forward", verbose=verbose)
  }

  if(verbose){
    graphics::plot(data$times, data$clapperState, type="l")
    graphics::points(x=minTime, data$clapperState[data$times==minTime])
    graphics::points(x=maxTime, data$clapperState[data$times==maxTime])
  }
  out <- subset(data, times >= minTime+offset & times <=maxTime+offset)
  out$times <- out$times-min(out$times)
  out
}


# main clipping function.
clipper <- function(data, verbose=FALSE, parallel=TRUE){
  `5-7` <- `6-8` <- `10-11` <- `9-12` <- NULL # to get rid of note errors
  file <- data[["pathMarkers"]]

  filteredMarkerData <- markerRead(file = file, verbose=FALSE)

  filteredMarkers <- extractMarkers(filteredMarkerData, c(0,1,2,3,4,5,6,7,8,9,10,11,12))
  filteredMarkers <- calculateDistances(filteredMarkers, c(5,7))
  filteredMarkers <- calculateDistances(filteredMarkers, c(6,8))
  filteredMarkers <- calculateDistances(filteredMarkers, c(10,11))
  filteredMarkers <- calculateDistances(filteredMarkers, c(9,12))
  filteredMarkers <- calculateDistances(filteredMarkers, c(0,1))
  filteredMarkers <- meanOnAxis(filteredMarkers, c(0,1,2,3,4), axis="Y")

  # average the clapper marker distances
  filteredMarkers$clapperState <- apply(subset(filteredMarkers, select = c(`5-7`,`6-8`,`10-11`,`9-12`)), 1, mean, na.rm=T)
  # ggplot(filteredMarkers) + geom_line(aes(x=times, y=clapperState), alpha = 1)  + xlim(5,15)
  alignedMarkers <- align(filteredMarkers, offset=1001/60000)
  alignedMarkers
}

clipWriter <- function(data, subjDir) {
  exp <- data[["Experiment"]]
  subj <- data[["subj"]]
  session <- data[["session"]]
  trial <- data[["trial"]]
  message(paste("Starting on:",paste(exp,subj,session,trial,sep="-"),sep=" "))

  alignedMarkers <- clipper(data)

  outFilename <- paste(subjDir, "/", paste(subj, session, trial,sep="-"),".csv", sep="")
  utils::write.csv(alignedMarkers, file = outFilename, row.names = FALSE)

  message(paste("Finished with:",paste(exp,subj,session,trial,sep="-"),sep=" "))

  alignedMarkers
}



makeOneElanFile <- function(videoFile){
  # runs through the video files, find csvs in the GRIP folder, generate csvs, generate elan files
  # Change the warn option so that warnings are displayd along with progress. There should be a better way to do this...
  oldWarn <- getOption("warn")
  options(warn = 1)

  # check if the videoFile exists
  if(!file.exists(videoFile)){
    warning(paste("The video file (",as.character(videoFile),") does not exist. No Elan files have been created.", sep = ""))
    return(1)
  }


  base <- strsplit(tail(strsplit(videoFile, "/", fixed=TRUE)[[1]], 1), ".", fixed=TRUE)[[1]][1]
  baseDir <- dirname(videoFile)
  subjNum <- basename(baseDir) # save the subject number for checkeing
  baseDir <- dirname(baseDir) # peal off the subject numbe
  clippedVideo <- basename(baseDir) # save the clipped video for checkeing
  baseDir <- dirname(baseDir) # this is the final basename

  if( !{grepl("\\d\\d\\d", subjNum)&grepl("Clipped Video", clippedVideo)} ) {
    stop(paste("Error, path to the video file (",videoFile,") is not what is expected. The video file should be in a folder ./Clipped Video/[subject number]", sep=""))
  }

  df <- data.frame(Experiment="GRIP",
                   subj = strsplit(base, "-", fixed=TRUE)[[1]][1],
                   session = strsplit(base, "-", fixed=TRUE)[[1]][2],
                   trial = strsplit(base, "-", fixed=TRUE)[[1]][3])
  df$pathMarkers <- paste(baseDir,"mocapData",df$Experiment, df$subj, df$session, df$trial,"Filtered Markers","Filtered Markers.txt",sep="/")

  #   csvDir <- paste(dirPath,"mocapCSVs",unique(df$subj),sep="/")
  csvDir <- paste(baseDir,"mocapCSVs",unique(df$subj),sep="/")

  dir.create(csvDir, recursive=TRUE, showWarnings=FALSE)

  #   elanDir <- paste(dirPath,"elanFilesOut",unique(df$subj),sep="/")
  elanDir <- paste(baseDir,"elanFilesOut",unique(df$subj),sep="/")
  dir.create(elanDir, recursive=TRUE, showWarnings=FALSE)

  markerData <- clipWriter(data=df, subjDir=csvDir)

  # "tracks" : [{"name": "clapper", "column": 36, "min":0, "max":200}]
  grip <- paste('{"name": "grip", "column": ',which( colnames(markerData)=="0-1" )-1,', "min":',minNotInf(markerData$`0-1`, na.rm=TRUE),', "max":',maxNotInf(markerData$`0-1`, na.rm=TRUE),'}', sep='')
  clapper <- paste('{"name": "clapper", "column": ',which( colnames(markerData)=="clapperState" )-1,', "min":',minNotInf(markerData$clapperState, na.rm=TRUE),', "max":',maxNotInf(markerData$clapperState, na.rm=TRUE),'}', sep='')
  meanY <- paste('{"name": "meanY", "column": ',which( colnames(markerData)=="mean-Y-0-1-2-3-4" )-1,', "min":',minNotInf(markerData$`mean-Y-0-1-2-3-4`, na.rm=TRUE),', "max":',maxNotInf(markerData$`mean-Y-0-1-2-3-4`, na.rm=TRUE),'}', sep='')
  tracks <- paste('"tracks" : [',grip,',',clapper,',',meanY,']')

  # check the times of the mocap data and the video data
  mocapDur <- maxNotInf(markerData$times)
  videoDur <- videoLength(shQuote(videoFile))
  if(!is.na(videoDur)){
    fuzz <- 0.5
    if(mocapDur > videoDur+fuzz){
      warning(paste("The motion capture data (",as.character(mocapDur)," seconds) is longer than the video data (",as.character(videoDur)," seconds). This is a sign that there is a problem with alignment.", sep = ""))
    }
    if(mocapDur+fuzz < videoDur){
      warning(paste("The video data (",as.character(videoDur)," seconds) is longer than the motion capture data (",as.character(mocapDur)," seconds). This is a sign that there is a problem with alignment.", sep = ""))
    }
  } else {
    warning("The video data duration could not be found. The motion capture and video durations were not checked against each other.")
  }


  pathToElanGen <- system.file("python","pyelan","elanGen.py", package = "mocapGrip", mustWork=TRUE)

  elanBasename <- base

  # find video files that match the video file supplied
  videoFilePaths <- list.files(path = dirname(videoFile), pattern=substr(basename(videoFile), 1, nchar(basename(videoFile)) - 4), full.names = TRUE)

  audioDir <- gsub("Clipped Video", "AUDIO", dirname(videoFile))
  audioFilePaths <- list.files(path = audioDir, pattern=substr(basename(videoFile), 1, nchar(basename(videoFile)) - 4), full.names = TRUE)

  mediaPaths <- c(videoFilePaths,audioFilePaths)
  media <- paste('["',paste(mediaPaths, collapse='","'),'"]', sep='')

  arugments <- c(pathToElanGen,paste(" \"",elanDir,"\" \"",elanBasename,"\" \'",media,"\'"," \'[{\"file\" : \"",paste(csvDir, "/", paste(df$subj, df$session, df$trial,sep="-"),".csv", sep=""),"\", ",tracks,"}]\'", sep=""))

  options(warn = oldWarn)
  system2("python", args = arugments)
  call
}



#' Make (blank) elan files from motion capture, video, and audio data.
#'
#' This function takes in a vector of video files, and writes out elan files with videos linked and synchronized motion capture data linked. It depends on the raw motion capture data, videos, and audio files being in set predetermined folders already.
#'
#' @param files A vector of video files
#' @return Nothing, not useful currently.
#'
#' @export
makeElanFiles <- function(files){
  lapply(Sys.glob(files), makeOneElanFile)

  return()
}
