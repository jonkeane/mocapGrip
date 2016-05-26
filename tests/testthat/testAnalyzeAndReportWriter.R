library(mocapGrip)
context("analysis functions")

test_that("equation (formula) generation works", {
  expect_equal(eqsGen2preds(outcome="maxGrip", predictor1="stickcmCentered", predictor2="fins"),
               c("maxGrip~stickcmCentered*fins+(1+stickcmCentered*fins|obsisSubj)",
                 "maxGrip~stickcmCentered*fins+(1+stickcmCentered+fins|obsisSubj)",
                 "maxGrip~stickcmCentered+fins+(1+stickcmCentered+fins|obsisSubj)"))
})

test_that("equation (formula) generation works", {
  expect_equal(eqsGen(modelMetadata$models$analyses$maxGrip.stickAsContinuous$variablesToUse, modelMetadata$models$modelStructures),
               c("interactionInPredAndGroup" = "maxGrip~stickcmCentered*fins+(1+stickcmCentered*fins|obsisSubj)",
                    "interactionInPred" = "maxGrip~stickcmCentered*fins+(1+stickcmCentered+fins|obsisSubj)",
                    "noInteraction" = "maxGrip~stickcmCentered+fins+(1+stickcmCentered+fins|obsisSubj)"))
})


modelsAction <- fitLMER(eqsGen2preds(outcome="maxGrip", predictor1="stickcmCentered", predictor2="fins"), data=pureReplication$action$data)
test_that("fitting lmer function returns the right shape, and handles warnings", {
  # these might change if optimizers change in lme
  expect_equal(modelsAction$`maxGrip~stickcmCentered*fins+(1+stickcmCentered*fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsAction$`maxGrip~stickcmCentered*fins+(1+stickcmCentered+fins|obsisSubj)`$converged, FALSE)
  expect_equal(modelsAction$`maxGrip~stickcmCentered+fins+(1+stickcmCentered+fins|obsisSubj)`$converged, TRUE)


  expect_silent(modelsEst <- fitLMER(eqsGen2preds(outcome="meanGrip", predictor1="stickcmCentered", predictor2="fins"), data=pureReplication$estimation$data))
  # these might change if optimizers change in lme
  expect_equal(modelsEst$`meanGrip~stickcmCentered*fins+(1+stickcmCentered*fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsEst$`meanGrip~stickcmCentered*fins+(1+stickcmCentered+fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsEst$`meanGrip~stickcmCentered+fins+(1+stickcmCentered+fins|obsisSubj)`$converged, TRUE)
})

test_that("fitting lmer function returns the right shape, and handles warnings", {
  expect_error(fitModels(dataSet="action", analysis="this is not a type", data=list()))
  expect_error(fitModels(dataSet="this is not a type", analysis="maxGrip.stickAsContinuous", data=list()))
})

test_that("the model chosing function works forwards", {
  expect_silent(modsChoseAction <- findTheBestModel(modelsAction))
  expect_equal(modsChoseAction$allModels, modelsAction)
  expect_equal(names(modsChoseAction$bestModel), "maxGrip~stickcmCentered*fins+(1+stickcmCentered*fins|obsisSubj)")
  expect_equal(names(modsChoseAction), c("bestModel", "allModels"))
  })

test_that("the model chosing function works backwords", {
  expect_silent(modsChoseAction <- findTheBestModel(modelsAction, last = TRUE))
  expect_equal(modsChoseAction$allModels, modelsAction)
  expect_equal(names(modsChoseAction), c("bestModel", "allModels"))
  expect_equal(names(modsChoseAction$bestModel), "maxGrip~stickcmCentered+fins+(1+stickcmCentered+fins|obsisSubj)")
})

### Setup dataModeled for test
dataModeled <- modelAllData(pureReplication)

# this takes the last model to fit as the best.
# In the current setup of modelMetadata.json, this is the simplest model.
dataModeledSimplest <- modelAllData(pureReplication, last = TRUE)


test_that("modelAllData runs through data", {
  expect_equal(names(dataModeled$action), c("data", "warnings", "analysesToRun", "analyses"))
  expect_equal(dataModeled$action$data, pureReplication$action$data)
  expect_equal(dataModeled$action$warnings, pureReplication$action$warnings)

  expect_equal(names(dataModeledSimplest$action), c("data", "warnings", "analysesToRun", "analyses"))
  expect_equal(dataModeledSimplest$action$data, pureReplication$action$data)
  expect_equal(dataModeledSimplest$action$warnings, pureReplication$action$warnings)
  })




context("report writer")

test_that("replaceText works with a selection of texts", {
  expect_equal(replaceText(list("##", "$title"), modelMetadata$dataSets$action$narrative),c("##", "Maximum grip aperture (on reach to grasp)"))
  expect_equal(replaceText(list("##", "$title", "$intro"), modelMetadata$dataSets$action$narrative),c("##", "Maximum grip aperture (on reach to grasp)", "The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system)."))
})

test_that("formatGatherReplacements works", {
  expect_equal(formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeled), list("dataSet" = "action",
                                                                                                  "analysis" = "maxGrip.stickAsContinuous",
                                                                     "title" = "Maximum grip aperture (on reach to grasp)",
                                                                     "intro" = "The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system).",
                                                                     "outcomeVariable" = "the maximum grip aperture (in mm)",
                                                                     "predictorVariables" = "\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open; where closed is the reference level)\n* the interaction between the size of the stick and configuration of fins",
                                                                     "includeInteractionInGroup" = "(including interactions)",
                                                                     "groupingVariable" = "by subject",
                                                                     "plotOutcome" = "maxGrip",
                                                                     "plotPredictor1" = "stickcmCentered+8",
                                                                     "plotPredictor2" = "fins",
                                                                     "formula" = "maxGrip ~ stickcmCentered * fins + (1 + stickcmCentered * fins | obsisSubj)"))

})


test_that("replaceText works with formatGatherReplacements", {
  ## check dataModeled
  expect_equal(replaceText(list("$title"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeled)),c("Maximum grip aperture (on reach to grasp)"))
  expect_equal(replaceText(list("$intro"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeled)),c("The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system)."))
  expect_equal(replaceText(list("$outcomeVariable"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeled)),c("the maximum grip aperture (in mm)"))
  expect_equal(replaceText(list("$predictorVariables"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeled)),c("\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open; where closed is the reference level)\n* the interaction between the size of the stick and configuration of fins"))
  expect_equal(replaceText(list("$includeInteractionInGroup"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeled)),c("(including interactions)"))
  expect_equal(replaceText(list("$groupingVariable"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeled)),c("by subject"))

  ## check dataModeledSimplest
  expect_equal(replaceText(list("$title"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeledSimplest)),c("Maximum grip aperture (on reach to grasp)"))
  expect_equal(replaceText(list("$intro"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeledSimplest)),c("The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system)."))
  expect_equal(replaceText(list("$outcomeVariable"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeledSimplest)),c("the maximum grip aperture (in mm)"))
  expect_equal(replaceText(list("$predictorVariables"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeledSimplest)),c("\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open; where closed is the reference level)"))
  expect_equal(replaceText(list("$includeInteractionInGroup"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeledSimplest)),c(""))
  expect_equal(replaceText(list("$groupingVariable"), formatGatherReplacements("action", "maxGrip.stickAsContinuous", dataModeledSimplest)),c("by subject"))
})

context("writeMarkdown")
test_that("writeMarkdown conforms to standard", {
  expect_silent(writeMarkdown(dataModeled, markdownPath = "./toTest.Rmd"))
  expect_equal(readLines("./toTest.Rmd"), readLines("./pureReplicationReport.Rmd"))
})

context("makeReport")
test_that("makeReport conforms to standard", {
  expect_output(newDataModeled <- makeReport(dataModeled, reportPath = "./toTestPR"))
  expect_equal(readLines("./toTestPR.Rmd"), readLines("./pureReplicationReport.Rmd"))
  # test only the length of the html documents due to rendering variabilities
  expect_equal(length(readLines("./toTestPR.html")), length(readLines("./pureReplicationReport.html")))
  expect_equal(newDataModeled, dataModeled)
})
