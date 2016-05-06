library(mocapGrip)
context("analysis functions")

test_that("equation (formula) generation works", {
  expect_equal(mocapGrip:::eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"),
               c("maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)",
                 "maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)",
                 "maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)"))
})

test_that("fitting lmer function returns the right shape, and handles warnings", {
  expect_warning(models <- mocapGrip:::fitLMER(mocapGrip:::eqsGen2preds(outcome="maxGrip", predictor1="stickcmScaled", predictor2="fins"), data=pureReplication$action$data))
  # these might change if optimizers change in lme
  expect_equal(models$`maxGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)`$converged, TRUE)
  expect_equal(models$`maxGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)`$converged, FALSE)
  expect_equal(models$`maxGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)


  expect_silent(models <- mocapGrip:::fitLMER(mocapGrip:::eqsGen2preds(outcome="meanGrip", predictor1="stickcmScaled", predictor2="fins"), data=pureReplication$estimation$data))
  # these might change if optimizers change in lme
  expect_equal(models$`meanGrip~stickcmScaled*fins+(1+stickcmScaled*fins|obsisSubj)`$converged, TRUE)
  expect_equal(models$`meanGrip~stickcmScaled*fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)
  expect_equal(models$`meanGrip~stickcmScaled+fins+(1+stickcmScaled+fins|obsisSubj)`$converged, TRUE)
})

test_that("fitting lmer function returns the right shape, and handles warnings", {
  expect_error(mocapGrip:::fitTheRightModels(type="this is not a type", data=list()))
})
