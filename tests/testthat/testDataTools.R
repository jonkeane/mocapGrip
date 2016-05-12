library(mocapGrip)
context("writeCSVsFromData")

test_that("writeCSVsFromData will overwrite", {
  expect_message(writeCSVsFromData(pureReplication))
})
test_that("writeCSVsFromData checks for existing files", {
  expect_error(writeCSVsFromData(pureReplication, overwrite=FALSE))
})
test_that("writeCSVsFromData will overwrite", {
  expect_message(writeCSVsFromData(pureReplication, overwrite=TRUE))
  unlink(c("./action.csv", "./estimation.csv")) # clean up
})
test_that("writeCSVsFromData accepts prefixes", {
  expect_message(writeCSVsFromData(pureReplication, namePrefix = "prefix"))
  unlink(c("./prefixaction.csv", "./prefixestimation.csv")) # clean up
})

context("displayModelsToRun")
test_that("display models works", {
  expect_message(displayAnalysesToRun(pureReplication))
})

