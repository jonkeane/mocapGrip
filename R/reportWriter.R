# add kinds of models?
# export(!)
makeReport <- function(data){
  # parse data?

  # fit models from analyze
  models <- fitLMER(eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"), data=pureReplication$action$data)

  # pick the best model (add warnings here?)

  # read/write template

  # render
  # change path
  rmarkdown::render("./inst/reportTemplateReplication.Rmd", params = list(data=mocapGrip::pureReplication))

}

