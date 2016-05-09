library(mocapGrip)
context("analysis functions")

test_that("equation (formula) generation works", {
  expect_equal(mocapGrip:::eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"),
               c("maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)",
                 "maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)",
                 "maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)"))
})

test_that("equation (formula) generation works", {
  expect_equal(mocapGrip:::eqsGen(modelStructure$models$analyses$action$variablesToUse, modelStructure$models$modelStructures),
               c("interactionInPredAndGroup" = "maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)",
                    "interactionInPred" = "maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)",
                    "noInteraction" = "maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)"))
})


modelsAction <- mocapGrip:::fitLMER(mocapGrip:::eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"), data=pureReplication$action$data)
test_that("fitting lmer function returns the right shape, and handles warnings", {
  # these might change if optimizers change in lme
  expect_equal(modelsAction$`maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsAction$`maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)`$converged, FALSE)
  expect_equal(modelsAction$`maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)


  expect_silent(modelsEst <- mocapGrip:::fitLMER(mocapGrip:::eqsGen2preds(outcome="meanGrip", predictor1="stickcmScaled", predictor2="fins"), data=pureReplication$estimation$data))
  # these might change if optimizers change in lme
  expect_equal(modelsEst$`meanGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsEst$`meanGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)
  expect_equal(modelsEst$`meanGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)
})

test_that("fitting lmer function returns the right shape, and handles warnings", {
  expect_error(mocapGrip:::fitModels(type="this is not a type", data=list()))
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

dataModeled <- modelAllData(pureReplication)

test_that("modelAllData runs through data", {
  expect_equal(names(dataModeled$action), c("data", "warnings", "models"))
  expect_equal(dataModeled$action$data, pureReplication$action$data)
  expect_equal(dataModeled$action$warnings, pureReplication$action$warnings)
})

context("report writer")

test_that("replaceText works with a selection of texts", {
  expect_equal(mocapGrip:::replaceText(list("##", "$title"), modelStructure$models$analyses$action$narrative),c("##", "Maximum Grip aperture (on reach to grasp)"))
  expect_equal(mocapGrip:::replaceText(list("##", "$title", "$intro"), modelStructure$models$analyses$action$narrative),c("##", "Maximum Grip aperture (on reach to grasp)", "The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system)."))
})

# test_that("reaplceText works with lists in the replacements", {
#   expect_equal(mocapGrip:::replaceText(list("$predictorVariables"), modelStructure$action),c("\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open, where closed is the reference level)\n* the interaction between the size of the stick and configuration of fins."))
# })

test_that("", {
  expect_equal(mocapGrip:::formatGatherReplacements("action", dataModeled), list("title" = "Maximum Grip aperture (on reach to grasp)",
                                                                                "intro" = "The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system).",
                                                                                "outcome" = "maximum grip aperture (in mm)",
                                                                                "predictorVariables" = "\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open, where closed is the reference level)\n* the interaction between the size of the stick and configuration of fins",
                                                                                "includeInteractionInGroup" = "(including interactions)",
                                                                                "groupingVariable" = "by subject"))

})

