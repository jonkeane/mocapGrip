library(mocapGrip)
context("writeCSVsFromData")

test_that("writeCSVsFromData checks for existing files", {
  expect_error(writeCSVsFromData(pureReplication, overwrite=FALSE))
})
test_that("writeCSVsFromData will overwrite", {
  expect_message(writeCSVsFromData(pureReplication, overwrite=TRUE))
})
test_that("writeCSVsFromData accepts prefixes", {
  expect_message(writeCSVsFromData(pureReplication, namePrefix = "prefix"))
  unlink(c("./prefixaction.csv", "./prefixestimation.csv"))
})

context("displayModelsToRun")
test_that("display models works", {
  expect_message(displayAnalysesToRun(pureReplication))
})

