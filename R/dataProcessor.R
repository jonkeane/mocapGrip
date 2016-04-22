############### from dataProcessor.R

# returns the max that is not infinity for when the mocap data returns infinty
maxNotInf <- function(vector, ...){
  	vector <- ifelse(is.infinite(vector),NA,vector)
	max(vector, ...)
}

# returns the min that is not infinity for when the mocap data returns (negative) infinty
minNotInf <- function(vector, ...){
  	vector <- ifelse(is.infinite(vector),NA,vector)
	min(vector, ...)
}

main <- function(videoFile){
  # runs through the video files, find csvs in the GRIP folder, generate csvs, generate elan files
  # Change the warn option so that warnings are displayd along with progress. There should be a better way to do this...
  oldWarn <- getOption("warn")
  options(warn = 1)

  base <- strsplit(tail(strsplit(videoFile, "/", fixed=TRUE)[[1]], 1), ".", fixed=TRUE)[[1]][1]
  df <- data.frame(Experiment="GRIP",
  subj = strsplit(base, "-", fixed=TRUE)[[1]][1],
  session = strsplit(base, "-", fixed=TRUE)[[1]][2],
  trial = strsplit(base, "-", fixed=TRUE)[[1]][3])
  df$pathMarkers <- paste("./mocapData/",df$Experiment, df$subj, df$session, df$trial,"Filtered Markers","Filtered Markers.txt",sep="/")

#   csvDir <- paste(dirPath,"mocapCSVs",unique(df$subj),sep="/")
  csvDir <- paste("mocapCSVs",unique(df$subj),sep="/")

  dir.create(csvDir, recursive=TRUE, showWarnings=FALSE)

#   elanDir <- paste(dirPath,"elanFilesOut",unique(df$subj),sep="/")
  elanDir <- paste("elanFilesOut",unique(df$subj),sep="/")
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


  pathToElanGen <- system.file("pyelan/elanGen.py", package = "mocapProcessor", mustWork=TRUE)

  elanBasename <- base

  # find video files that match the video file supplied
  videoFilePaths <- list.files(path = dirname(videoFile), pattern=substr(basename(videoFile), 1, nchar(basename(videoFile)) - 4), full.names = TRUE)

  audioDir <- gsub("Clipped Video", "AUDIO", dirname(videoFile))
  audioFilePaths <- list.files(path = audioDir, pattern=substr(basename(videoFile), 1, nchar(basename(videoFile)) - 4), full.names = TRUE)

  mediaPaths <- c(videoFilePaths,audioFilePaths)
  media <- paste('["',paste(mediaPaths, collapse='","'),'"]', sep='')

  call <- paste("python ",pathToElanGen," \"",elanDir,"\" \"",elanBasename,"\" \'",media,"\'"," \'[{\"file\" : \"",paste(csvDir, "/", paste(df$subj, df$session, df$trial,sep="-"),".csv", sep=""),"\", ",tracks,"}]\'", sep="")

  options(warn = oldWarn)
  system(call)
  call
}
