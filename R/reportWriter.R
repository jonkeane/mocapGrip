# add kinds of models?
# export(!)
makeReport <- function(data){
  # parse data?

  # fit models from analyze
  models <- fitLMER(eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"), data=mocapGrip::pureReplication$action$data)

  # pick the best model (add warnings here?)

  # read/write template

  # render
  # change path
  rmarkdown::render("./inst/reportTemplateReplication.Rmd", params = list(data=mocapGrip::pureReplication))

}

# split the text, then join
cleanText <- function(text, reps){
  # split on the special sequence of characters.
  text <- strsplit(text, split="<>")[[1]]
  paste0(replaceText(text, reps = reps))
}

# replace the text with text from replacements
replaceText <- Vectorize(function(text, reps){
  print(text)
  if(!grepl("\\$.*", text)){
    # if the variable character is not there, return the text.
    return(text)
  }
  replText <- reps[[substring(text, 2)]]
  if(length(replText)>1){
    return(paste0("\n* ", replText,collapse = "\n* "))
  }
  return(replText)
}, vectorize.args = "text", USE.NAMES = FALSE)
#
# modelStructure <- jsonlite::fromJSON("./inst/modelStructure.json")
# analysisSkel <- readLines("./inst/analysisSkeleton.rmd")
#
# cleanText(analysisSkel[7], modelStructure$action)
