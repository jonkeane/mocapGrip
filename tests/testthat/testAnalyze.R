library(mocapGrip)
context("analysis functions")

test_that("equation (formula) generation works", {
  expect_equal(mocapGrip:::eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"),
               c("maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)",
                 "maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)",
                 "maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)"))
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

test_that("modelAllData runs through data", {
  expect_warning(dataModeled <- modelAllData(pureReplication))
  expect_equal(names(dataModeled$action), c("data", "warnings", "models"))
  expect_equal(dataModeled$action$data, pureReplication$action$data)
  expect_equal(dataModeled$action$warnings, pureReplication$action$warnings)
})
