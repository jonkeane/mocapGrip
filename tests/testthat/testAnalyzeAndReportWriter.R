library(mocapGrip)
context("analysis functions")

test_that("equation (formula) generation works", {
  expect_equal(eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"),
               c("maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)",
                 "maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)",
                 "maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)"))
})

test_that("equation (formula) generation works", {
  expect_equal(eqsGen(modelStructure$models$analyses$action$variablesToUse, modelStructure$models$modelStructures),
               c("interactionInPredAndGroup" = "maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)",
                    "interactionInPred" = "maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)",
                    "noInteraction" = "maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)"))
})


modelsAction <- fitLMER(eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"), data=pureReplication$action$data)
test_that("fitting lmer function returns the right shape, and handles warnings", {
  # these might change if optimizers change in lme
  expect_equal(modelsAction$`maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsAction$`maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)`$converged, FALSE)
  expect_equal(modelsAction$`maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)


  expect_silent(modelsEst <- fitLMER(eqsGen2preds(outcome="meanGrip", predictor1="stickcmScaled", predictor2="fins"), data=pureReplication$estimation$data))
  # these might change if optimizers change in lme
  expect_equal(modelsEst$`meanGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsEst$`meanGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsEst$`meanGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)
})

test_that("fitting lmer function returns the right shape, and handles warnings", {
  expect_error(fitModels(type="this is not a type", data=list()))
})

test_that("the model chosing function works forwards", {
  expect_silent(modsChoseAction <- findTheBestModel(modelsAction))
  expect_equal(modsChoseAction$allModels, modelsAction)
  expect_equal(names(modsChoseAction$bestModel), "maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)")
  expect_equal(names(modsChoseAction), c("bestModel", "allModels"))
  })

test_that("the model chosing function works backwords", {
  expect_silent(modsChoseAction <- findTheBestModel(modelsAction, last = TRUE))
  expect_equal(modsChoseAction$allModels, modelsAction)
  expect_equal(names(modsChoseAction), c("bestModel", "allModels"))
  expect_equal(names(modsChoseAction$bestModel), "maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)")
})


### Setup dataModeled for test
dataModeled <- modelAllData(pureReplication)

# this takes the last model to fit as the best.
# In the current setup of modelStructure.json, this is the simplest model.
dataModeledSimplest <- modelAllData(pureReplication, last = TRUE)


test_that("modelAllData runs through data", {
  expect_equal(names(dataModeled$action), c("data", "warnings", "models"))
  expect_equal(dataModeled$action$data, pureReplication$action$data)
  expect_equal(dataModeled$action$warnings, pureReplication$action$warnings)

  expect_equal(names(dataModeledSimplest$action), c("data", "warnings", "models"))
  expect_equal(dataModeledSimplest$action$data, pureReplication$action$data)
  expect_equal(dataModeledSimplest$action$warnings, pureReplication$action$warnings)
  })

context("report writer")

test_that("replaceText works with a selection of texts", {
  expect_equal(replaceText(list("##", "$title"), modelStructure$models$analyses$action$narrative),c("##", "Maximum Grip aperture (on reach to grasp)"))
  expect_equal(replaceText(list("##", "$title", "$intro"), modelStructure$models$analyses$action$narrative),c("##", "Maximum Grip aperture (on reach to grasp)", "The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system)."))
})

test_that("replaceText works with formatGatherReplacements", {
  ## check dataModeled
  expect_equal(replaceText(list("$title"), formatGatherReplacements("action", dataModeled)),c("Maximum Grip aperture (on reach to grasp)"))
  expect_equal(replaceText(list("$intro"), formatGatherReplacements("action", dataModeled)),c("The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system)."))
  expect_equal(replaceText(list("$outcomeVariable"), formatGatherReplacements("action", dataModeled)),c("maximum grip aperture (in mm)"))
  expect_equal(replaceText(list("$predictorVariables"), formatGatherReplacements("action", dataModeled)),c("\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open, where closed is the reference level)\n* the interaction between the size of the stick and configuration of fins"))
  expect_equal(replaceText(list("$includeInteractionInGroup"), formatGatherReplacements("action", dataModeled)),c("(including interactions)"))
  expect_equal(replaceText(list("$groupingVariable"), formatGatherReplacements("action", dataModeled)),c("by subject"))

  ## check dataModeledSimplest
  expect_equal(replaceText(list("$title"), formatGatherReplacements("action", dataModeledSimplest)),c("Maximum Grip aperture (on reach to grasp)"))
  expect_equal(replaceText(list("$intro"), formatGatherReplacements("action", dataModeledSimplest)),c("The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system)."))
  expect_equal(replaceText(list("$outcomeVariable"), formatGatherReplacements("action", dataModeledSimplest)),c("maximum grip aperture (in mm)"))
  expect_equal(replaceText(list("$predictorVariables"), formatGatherReplacements("action", dataModeledSimplest)),c("\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open, where closed is the reference level)"))
  expect_equal(replaceText(list("$includeInteractionInGroup"), formatGatherReplacements("action", dataModeledSimplest)),c(""))
  expect_equal(replaceText(list("$groupingVariable"), formatGatherReplacements("action", dataModeledSimplest)),c("by subject"))
})

test_that("formatGatherReplacements works", {
  expect_equal(formatGatherReplacements("action", dataModeled), list("analysis" = "action",
                                                                                 "title" = "Maximum Grip aperture (on reach to grasp)",
                                                                                 "intro" = "The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system).",
                                                                                 "outcomeVariable" = "maximum grip aperture (in mm)",
                                                                                 "predictorVariables" = "\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open, where closed is the reference level)\n* the interaction between the size of the stick and configuration of fins",
                                                                                 "includeInteractionInGroup" = "(including interactions)",
                                                                                 "groupingVariable" = "by subject",
                                                                                 "plotOutcome" = "maxGrip",
                                                                                 "plotPredictor1" = "stickcmScaled",
                                                                                 "plotPredictor2" = "fins",
                                                                                 "formula" = "maxGrip ~ stickcmScaled * fins + (1 + stickcmScaled * fins | obsisSubj)"))

})

writeMarkdown(dataModeled, markdownPath = "./toTest.Rmd")
# paused for now as the report is developed.
# test_that("writeMarkdown conforms to standard", {
#   expect_equal(readLines("./toTest.Rmd"), readLines("./pureReplicationReport.Rmd"))
# })
